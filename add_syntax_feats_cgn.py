#!/usr/bin/env python2

import sys
import re
import tempfile
import textgrid
import codecs
from lxml import etree as ET
from HTMLParser import HTMLParser
from htmlentitydefs import entitydefs
import multiprocessing
import gzip

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
cgn_path = "/Volumes/tensusers/timzee/cgn/Annotations/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/Annotations/"
cgn_path2 = "/Volumes/bigdata2/corpora2/CGN2/data/annot/" if sys.platform == "darwin" else "/vol/bigdata2/corpora2/CGN2/data/annot/"
tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

h = HTMLParser()
# get rid of predefined XML entities; these are handled by XML parser
del entitydefs["amp"]
del entitydefs["quot"]
del entitydefs["lt"]
del entitydefs["gt"]

input_file_path = tens_path + "cgn/comp-a_s_ndl_static_final.csv"
with codecs.open(input_file_path, "r", "utf-8") as f:
    input_file = f.readlines()

running_cores = 0

# Do not run with more than 5 cores.
num_cores = 10
num_index_lines = len(input_file)
# num_index_lines = 1465779
core_dict = {}
for i in range(num_cores):
    core_dict[str(i + 1 + running_cores)] = {}
    core_dict[str(i + 1 + running_cores)]["start"] = int(num_index_lines / num_cores) * i + 1
    if i + 1 != num_cores:
        core_dict[str(i + 1 + running_cores)]["end"] = int(num_index_lines / num_cores) * (i + 1)
    else:
        core_dict[str(i + 1 + running_cores)]["end"] = num_index_lines


def createTemp(tg_path):
    with codecs.open(tg_path, "r", encoding="utf-8") as f:
        tg_string = f.read()
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(tg_string.encode("utf-8"))
    tempf.flush()
    return tempf


def loadTextGrid(tg_path):
    tg = textgrid.TextGrid()
    with createTemp(tg_path) as tempf:
        tg.read(tempf.name)
    return tg


def getSpeaker(fp, tier):
    tg = loadTextGrid(cgn_path + "ort/comp-a/" + fp.split("/")[-1] + ".ort")
    spkr = tg.getNames()[int(tier) - 1]
    return spkr


def getSentenceInfo(rt, spkr, from_t, to_t, wrd):
    candidates = []
    wrd = "'s" if wrd == "da's" else wrd
    for child in rt:
        if child.tag == "tau":
            if child.attrib["s"] == spkr:
                for word in child:
                    if word.attrib["tb"] == from_t and word.attrib["te"] == to_t:
                        if h.unescape(word.attrib["w"]) == wrd:
                            candidates.append((int(word.attrib["ref"].split(".")[1]), int(word.attrib["ref"].split(".")[2])))
    # in case later sentence is positioned above earlier sentence in xml
    match = sorted(candidates)[0] if len(candidates) > 0 else (None, None)
    return match


def getPOS(rt, s_ind, w_ind, wrd):
    pw_list = rt.findall(".//pw[@ref='{}']".format(".".join([rt.attrib["ref"], str(s_ind), str(w_ind)])))
    # if list is empty --> cross-referencing mistake in CGN; e.g. fv701108.12.8 in .skp = fv701108.13.8 in .tag
    if len(pw_list) == 0:
        print("CROSS-REFERENCE DOES NOT EXIST IN .TAG")
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
    pw = pw_list[0]
    # we check for wordform, in case cross-reference does exist but is for another word due to CGN mistake
    if not h.unescape(pw.attrib["w"]) in wrd:
        print("WORDFORM MISMATCH IN CROSS-REFERENCE")
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
    # ignore words that are part of multi-word-units
    if pw.attrib["mwu"] == "1":
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
    # Get dependency grammar features
    pau = rt.findall(".//pau[@ref='{}']".format(".".join([rt.attrib["ref"], str(s_ind)])))[0]
    pau_children = list(pau)
    pw_index = pau_children.index(pw)
    # get closest incoming and outgoing link to the right
    dep_index_out = int(pw.attrib["depindex"])
    dep_out = pw.attrib["dep"]
    out_tup = (dep_index_out, dep_out, True)
    origindex = pw.attrib["origindex"]
    dep_index_in = 0
    dep_in = ""
    for pw_right in pau_children[pw_index:]:
        if pw_right.attrib["depindex"] == origindex:
            dep_index_in = int(pw_right.attrib["origindex"])
            dep_in = pw_right.attrib["dep"]
            break
    in_tup = (dep_index_in, dep_in, False)
    if dep_index_in and dep_index_out > int(origindex):  # if both incoming and outgoing right-adjacent links exist
        f1_index, f1, outgoing = min(out_tup, in_tup)
    else:
        if dep_index_in:  # only incoming
            f1 = dep_in
            f1_index = dep_index_in
            outgoing = False
        elif dep_index_out > int(origindex):  # only outgoing
            f1 = dep_out
            f1_index = dep_index_out
            outgoing = True
        else:
            f1 = "end"
            f1_index = int(origindex) + 1
            outgoing = True
    # now check for right-going and left-going links above closest right-adjacent link
    f2 = 0
    left_enc1 = 1000
    left_enc2 = 1000
    mwu = False
    prev_origindex = -1
    for pw_left in pau_children[:pw_index]:  # right-going links from preceding words
        mwu = pw_left.attrib["mwu"] == "1"
        if int(pw_left.attrib["depindex"]) >= f1_index:
            if mwu and prev_origindex == int(pw_left.attrib["origindex"]):  # ignore links from non-initial words in mwus
                pass
            else:
                f2 += 1
        # Number of words enclosed by shortest left-adjacent link
        if pw_left.attrib["depindex"] == origindex:
            left_enc1 = pw_index - pau_children.index(pw_left) - 1
            left_enc1 = 0 if left_enc1 < 0 else left_enc1
        if int(pw_left.attrib["origindex"]) == dep_index_out:
            left_enc2 = pw_index - pau_children.index(pw_left) - 1
            left_enc2 = 0 if left_enc2 < 0 else left_enc2
        prev_origindex = int(pw_left.attrib["origindex"])
    if left_enc1 == left_enc2 == 1000:
        f3 = 0
    else:
        f3 = min(left_enc1, left_enc2)
#    print("f2 A", f2)
    if not outgoing:
        if dep_index_out > f1_index:  # right-going link from current word
            f2 += 1
#    print("f2 B", f2)
    num_link = 0
    num_right_link = 0
    f4 = 0
    mwu = False
    prev_origindex = -1
    for pw_right in pau_children[pw_index + 1:]:
#        print(pw_right.attrib["w"], pw_right.attrib["depindex"], origindex)
        mwu = pw_right.attrib["mwu"] == "1"
        if (int(pw_right.attrib["depindex"]) <= int(origindex)) and (int(pw_right.attrib["depindex"]) != 0):  # left-going links from current and subsequent words
            num_link += 1
            if mwu and prev_origindex == int(pw_right.attrib["origindex"]):  # ignore links from non-initial words in mwus
                pass
            else:
                if not outgoing:
                    if num_link > 1:
                        f2 += 1
                else:
                    f2 += 1
        # Number of words enclosed by shortest right-adjacent link
        if not outgoing:
            if int(pw_right.attrib["depindex"]) == int(origindex) and num_link == 1:
                right_enc = pau_children.index(pw_right) - pw_index - 1
                f4 = right_enc if right_enc >= 0 else 0
        elif outgoing and f1 != "end":
            if int(pw_right.attrib["origindex"]) == f1_index:
                num_right_link += 1
                if num_right_link == 1:
                    right_enc = pau_children.index(pw_right) - pw_index - 1
                    f4 = right_enc if right_enc >= 0 else 0
        else:
            f4 = 0
        prev_origindex = int(pw_right.attrib["origindex"])
#    print("f2 C", f2)
    # Number of links going left/right from current word
    if dep_index_out == 0:
        f5 = 0
        f6 = 0
    elif dep_index_out > int(origindex):
        f5 = 0
        f6 = 1
    else:
        f5 = 1
        f6 = 0
    # Number of links going left/right from next word
    if pw_index + 1 == len(pau_children):
        f7 = 0
        f8 = 0
    else:
        next_pw = pau_children[pw_index + 1]
        dep_index_out_next = int(next_pw.attrib["depindex"])
        origindex_next = int(next_pw.attrib["origindex"])
        if dep_index_out_next == 0:
            f7 = 0
            f8 = 0
        elif dep_index_out_next > origindex_next:
            f7 = 0
            f8 = 1
        else:
            f7 = 1
            f8 = 0
    return [str(feat) for feat in [f1, f2, f3, f4, f5, f6, f7, f8]]


def parseLine(f_path, from_time, to_time, tier, new_file, word):
    global old_speaker
    global hnr
    speaker = getSpeaker(f_path, tier)
    if new_file:
        with gzip.open(cgn_path2 + "xml/skp-ort/comp-" + f_path + ".skp.gz") as h:
            skp_gz = h.read()
        skp_txt = codecs.decode(skp_gz, "ascii")
#        skp_txt = skp_txt.encode("utf-8")
        for ent in entitydefs:
            skp_txt = re.sub(r"&{};".format(ent), entitydefs[ent].decode("latin-1"), skp_txt)
#            skp_txt.replace("&" + ent + ";", entitydefs[ent].decode("latin-1"))
#        print(skp_txt)
        global skp_root
        skp_root = ET.fromstring(skp_txt)
        with codecs.open(cgn_path + "tag/comp-a/" + f_path.split("/")[-1] + ".tag", "r", "utf-8") as h:
            tag_txt = h.read()
#        tag_txt = codecs.decode(tag_gz, "ascii")
#        for ent in entitydefs:
#            tag_txt = re.sub(r"&{};".format(ent), entitydefs[ent].decode("latin-1"), tag_txt)
        global tag_root
        tag_root = ET.fromstring(tag_txt)
#        hnr = getHNR(f_path, chan, tier)
    else:
        if speaker != old_speaker:
            pass
#            hnr = getHNR(f_path, chan, tier)
    old_speaker = speaker

    word = re.sub(r'\*[a-z]', '', re.sub(r"[!?.,:;\t\n\r]*", "", word))
    sent_i, word_sent_i = getSentenceInfo(skp_root, speaker, from_time, to_time, word)
    syntax_feats = getPOS(tag_root, sent_i, word_sent_i, word)
    return syntax_feats


def readWriteMetaData(core_num="", start_line=1, end_line=num_index_lines):
    """Writes file for each process."""
#    if select.select([sys.stdin, ], [], [], 0.0)[0]:
#        print("Reading from Standard Input")
#        f = codecs.getreader('utf-8')(sys.stdin)
#    else:
    print("Reading from file")
    f = codecs.open(input_file_path, "r", "utf-8")
    with codecs.open(tens_path + "cgn/syntax_s" + core_num + ".csv", "w", "utf-8") as g:
        output_header = "wav,chan,chunk_start,chunk_end,tier,word_chunk_i,sent_i,word_sent_i,word_ort,word_phon,num_phon,phon_pron,prev_phon,prev_phon_pron,next_phon,next_phon_pron,word_pos,word_class,type_of_s,speaker,per_mil_wf,log_wf,lex_neb,lex_neb_freq,ptan,ptaf,cow_wf,next_word,next_wf,bigram_f,prev_word,prev_wf,prev_bigram_f,num_syl,word_stress,ndl_boundary_diph,other_ndl_cues,s_dur,kal_start,kal_end,s_cog_full,s_cog_window,proportion_voiced,proportion_voiced2,mean_hnr,speech_rate_pron,base_dur,num_syl_pron,num_cons_pron,speaker_sex,birth_year,next_phon_dur,prev_phon_dur,prev_mention,phrase_final,nn_start,nn_end,nn_start_score,nn_end_score,syntax_f1,syntax_f2,syntax_f3,syntax_f4,syntax_f5,syntax_f6,syntax_f7,syntax_f8\n"
        g.write(output_header)
        old_f_path = ""
        for l_num, line in enumerate(f, 1):
            if l_num >= start_line and l_num <= end_line:
                print("core " + core_num + ": " + line[:-1].encode("utf-8"))
                line_l = line[:-1].split(",")
                if line_l[1] == "chan":
                    continue
                file_path = line_l[0]
                c_start, c_end, tg_tier = line_l[2:5]
                c_start = "{0:.3f}".format(float(c_start))
                c_end = "{0:.3f}".format(float(c_end))
                w = line_l[8]
                new = True if old_f_path != file_path else False
                syn_feats = parseLine(file_path, c_start, c_end, tg_tier, new, w)
                g.write(",".join(line_l + syn_feats) + "\n")
                old_f_path = file_path[:]
    f.close()


def multiProcess():
    jobs = []
    for core in range(num_cores):
        core_n = str(core + 1 + running_cores)
        s_line = core_dict[core_n]["start"]
        e_line = core_dict[core_n]["end"]
        p = multiprocessing.Process(target=readWriteMetaData, args=[core_n, s_line, e_line])
        jobs.append(p)
        p.start()
    for job in jobs:
        job.join()
    # combine separate files
    with codecs.open(tens_path + "cgn/syntax_s_comb_comp-a.csv", "w", encoding="utf-8") as g:
        for core in range(num_cores):
            core_n = str(core + 1 + running_cores)
            with codecs.open(tens_path + "cgn/syntax_s" + core_n + ".csv", "r", encoding="utf-8") as f:
                for fln, f_line in enumerate(f, 1):
                    if not (core > 0 and fln == 1):
                        g.write(f_line)


if __name__ == '__main__':
    multiProcess()
