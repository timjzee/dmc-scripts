#!/usr/bin/env python2

import sys
import select
import re
import gzip
import tempfile
import textgrid
import codecs
from lxml import etree as ET
from HTMLParser import HTMLParser
from htmlentitydefs import entitydefs
import multiprocessing


tens_path = "/Volumes/tensusers/timzee/cgn/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/"
cgn_path = "/Volumes/bigdata2/corpora2/CGN2/data/annot/" if sys.platform == "darwin" else "/vol/bigdata/corpora2/CGN2/data/annot/"
tz_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/home/timzee/Docs/"

segment = "s"
h = HTMLParser()
# get rid of predefined XML entities; these are handled by XML parser
del entitydefs["amp"]
del entitydefs["quot"]
del entitydefs["lt"]
del entitydefs["gt"]

num_cores = 14
num_index_lines = 1465779
core_dict = {}
for i in range(num_cores):
    core_dict[str(i + 1)] = {}
    core_dict[str(i + 1)]["start"] = int(num_index_lines / num_cores) * i + 1
    if i + 1 != num_cores:
        core_dict[str(i + 1)]["end"] = int(num_index_lines / num_cores) * (i + 1)
    else:
        core_dict[str(i + 1)]["end"] = num_index_lines

kaldi_lex = {}
with codecs.open(tz_path + "lexicon_from_MARIO.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        kaldi_lex[entry] = pron


def createTemp(tg_path):
    with gzip.open(tg_path) as f:
        gz_bytes = f.read()
    tg_string = codecs.decode(gz_bytes, "iso-8859-1")
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(tg_string.encode("utf-8"))
    tempf.flush()
    return tempf


def loadTextGrid(tg_path):
    tg = textgrid.TextGrid()
    with createTemp(tg_path) as tempf:
        tg.read(tempf.name, encoding="utf-8")
    return tg


def getSpeaker(fp, tier):
    tg = loadTextGrid(cgn_path + "text/ort/comp-" + fp + ".ort.gz")
    spkr = tg.getNames()[int(tier) - 1]
    return spkr


def checkCanonical(w, position):
    # ignore final /s/s in unfinished words / speaker noises
    w = re.sub(r"[!?.,:;\t\n\r]", "", w.lower())
    if re.search(r"\*[^ ]*", w):
        return "NA"
    if w not in kaldi_lex:
        return None
    phonlist = kaldi_lex[w].split(" ")
    return phonlist[position]


def getSentenceInfo(rt, spkr, from_t, to_t, wrd):
    candidates = []
    wrd = "'s" if wrd == "da's" else wrd
    for child in rt:
        if child.tag == "tau":
            if child.attrib["s"] == spkr:
                for word in child:
                    if word.attrib["tb"] == from_t and word.attrib["te"] == to_t:
#                        print(wrd.encode("utf-8"), h.unescape(word.attrib["w"]).encode("utf-8"))
                        if h.unescape(word.attrib["w"]) == wrd:
                            candidates.append((int(word.attrib["ref"].split(".")[1]), int(word.attrib["ref"].split(".")[2])))
    # in case later sentence is positioned above earlier sentence in xml
    match = sorted(candidates)[0]
    return match


def getPOS(rt, s_ind, w_ind, wrd):
    pw_list = rt.findall(".//pw[@ref='{}']".format(".".join([rt.attrib["ref"], str(s_ind), str(w_ind)])))
    # if list is empty --> cross-referencing mistake in CGN; e.g. fv701108.12.8 in .skp = fv701108.13.8 in .tag
    if len(pw_list) == 0:
        print("CROSS-REFERENCE DOES NOT EXIST IN .TAG")
        return "NA", "NA", "NA"
    pw = pw_list[0]
    # we check for wordform, in case cross-reference does exist but is for another word due to CGN mistake
    if not h.unescape(pw.attrib["w"]) in wrd:
        print("WORDFORM MISMATCH IN CROSS-REFERENCE")
        return "NA", "NA", "NA"
    w_pos = re.sub(",", ";", pw.attrib["pos"])
    pos_attr = re.search(r"(?<=\().*(?=\))", w_pos).group().split(";")
    word_class = re.search(r".+(?=\()", w_pos).group()
    if word_class == "N":
        if "mv" in pos_attr:
            type_of_s = "PL"
        elif "gen" in pos_attr:
            type_of_s = "GEN"
        else:
            type_of_s = "S"
    elif word_class == "ADJ":
        if "met-s" in pos_attr:
            type_of_s = "DER"
        elif "dim" in pos_attr:
            type_of_s = "OTHER"
        else:
            type_of_s = "S"
    elif word_class == "WW":
        type_of_s = "S"
    elif word_class == "TW":
        if "prenom" in pos_attr:
            if "bijz" in pos_attr:
                type_of_s = "GEN"
            else:
                type_of_s = "S"
        if "nom" in pos_attr:
            type_of_s = "OTHER"
        else:
            type_of_s = "S"
    elif word_class == "VNW":
        if "gen" in pos_attr:
            type_of_s = "GEN"
        else:
            type_of_s = "S"
    elif word_class == "LID":
        type_of_s = "OTHER"
    elif word_class == "SPEC" or word_class == "LET":
        type_of_s = "NA"
    else:
        type_of_s = "S"
    return w_pos, word_class, type_of_s


def parseLine(f_path, chan, from_time, to_time, ort, new_file):
    speaker = getSpeaker(f_path, chan)
    if new_file:
        with gzip.open(cgn_path + "xml/skp-ort/comp-" + f_path + ".skp.gz") as h:
            skp_gz = h.read()
        skp_txt = codecs.decode(skp_gz, "ascii")
#        skp_txt = skp_txt.encode("utf-8")
        for ent in entitydefs:
            skp_txt = re.sub(r"&{};".format(ent), entitydefs[ent].decode("latin-1"), skp_txt)
#            skp_txt.replace("&" + ent + ";", entitydefs[ent].decode("latin-1"))
#        print(skp_txt)
        global skp_root
        skp_root = ET.fromstring(skp_txt)
#        print(ET.tostring(skp_root))
        with gzip.open(cgn_path + "xml/tag/comp-" + f_path + ".tag.gz") as h:
            tag_gz = h.read()
        tag_txt = codecs.decode(tag_gz, "ascii")
        for ent in entitydefs:
            tag_txt = re.sub(r"&{};".format(ent), entitydefs[ent].decode("latin-1"), tag_txt)
        global tag_root
        tag_root = ET.fromstring(tag_txt)
    # clean up ort
    word_list = [word for word in ort.split(" ") if word not in ["", " "]]
    oov = False
    output_lines = []
    for counter, word in enumerate(word_list, 1):
        word = re.sub(r"[!?.,:;\t\n\r]*", "", word)
        # check for segment / oov
        seg_var = checkCanonical(word, -1)
        if not seg_var:
            oov = True
        elif seg_var != segment:
            continue
        else:
            word_chunk_i = counter
            sent_i, word_sent_i = getSentenceInfo(skp_root, speaker, from_time, to_time, word)
            found = skp_root.findall(".//tw[@ref='{}']".format(".".join([f_path.split("/")[-1], str(sent_i), str(word_sent_i)])))[0]
            parent = skp_root.findall("./tau[@ref='{}']".format(".".join([f_path.split("/")[-1], str(sent_i)])))[0]
            parent.remove(found)
            next_phon = "SIL" if counter == len(word_list) else checkCanonical(word_list[counter], 0)
            next_phon = "NA" if next_phon is None else next_phon
            word_pos, word_class, type_of_s = getPOS(tag_root, sent_i, word_sent_i, word)
            output_lines.append([str(word_chunk_i), str(sent_i), str(word_sent_i), word, next_phon, word_pos, word_class, type_of_s, speaker])
            print(word, word_pos, type_of_s)
    return [",".join([f_path, chan, from_time, to_time, str(oov)] + ol) + "\n" for ol in output_lines] if len(output_lines) != 0 else []


def readWriteMetaData(core_num="", start_line=1, end_line=num_index_lines):
#    if select.select([sys.stdin, ], [], [], 0.0)[0]:
#        print("Reading from Standard Input")
#        f = codecs.getreader('utf-8')(sys.stdin)
#    else:
    print("Reading from file")
    f = codecs.open(tens_path + "cgn_ort_index_210119.txt", "r", "utf-8")
    with codecs.open(tens_path + "all_s" + core_num + ".csv", "w", "utf-8") as g:
        output_header = "wav,chan,chunk_start,chunk_end,oov_in_chunk,word_chunk_i,sent_i,word_sent_i,word_ort,next_phon,word_pos,word_class,type_of_s,speaker\n"
        g.write(output_header)
        old_f_path = ""
        for l_num, line in enumerate(f, 1):
            if l_num >= start_line and l_num <= end_line:
                print("core " + core_num + ": " + line[:-1].encode("utf-8"))
                if line.split(",")[2] == "from":
                    continue
                file_path, channel, f_time, t_time, orth = line[:-1].split(",")
                new = True if old_f_path != file_path else False
                write_lines = parseLine(file_path, channel, f_time, t_time, orth, new)
                for wl in write_lines:
                    g.write(wl)
                old_f_path = file_path[:]
    f.close()


def multiProcess():
    for core in range(num_cores):
        core_n = str(core + 1)
        s_line = core_dict[core_n]["start"]
        e_line = core_dict[core_n]["end"]
        p = multiprocessing.Process(target=readWriteMetaData, args=[core_n, s_line, e_line])
        p.start()
    # combine separate files
    with codecs.open(tens_path + "all_s_combined.csv", "w", encoding="utf-8") as g:
        for core in range(num_cores):
            core_n = str(core + 1)
            with codecs.open(tens_path + "all_s" + core_n + ".csv", "r", encoding="utf-8") as f:
                for fln, f_line in enumerate(f, 1):
                    if not (core > 0 and fln == 1):
                        g.write(f_line)


if __name__ == '__main__':
    multiProcess()
