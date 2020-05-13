#!/usr/bin/env python2

import sys
import select
import re
import tempfile
import textgrid
import codecs
from lxml import etree as ET
from HTMLParser import HTMLParser
from htmlentitydefs import entitydefs
import multiprocessing
import subprocess
import glob

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/Annotations/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFADVcorpus/Annotations/"
tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

ndl_window_start = -2
ndl_window_end = 2
n_phones = 3

segment = "s"
h = HTMLParser()
# get rid of predefined XML entities; these are handled by XML parser
del entitydefs["amp"]
del entitydefs["quot"]
del entitydefs["lt"]
del entitydefs["gt"]

input_file_path = tens_path + "IFADVcorpus/ifadv_index2.txt"
with codecs.open(input_file_path, "r", "utf-8") as f:
    input_file = f.readlines()

running_cores = 0

# Do not run with more than 5 cores.
num_cores = 7
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

print("Loading KALDI lexicon")
kaldi_lex = {}
with codecs.open(tz_path + "clst-asr-fa/lexicon_comp-acd-ifadv-ecsd-ko.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        kaldi_lex[entry] = pron

print("Loading SUBTLEX")
subtlex = {}
with codecs.open(tens_path + "other/SUBTLEX-NL.txt", "r", "utf-8") as f:
    for line in f:
        line_list = line[:-1].split("\t")
        word = line_list[0]
        subtlexwf = line_list[6]
        lg10wf = line_list[7]
        subtlex[word] = [subtlexwf, lg10wf]

print("Loading ClearPond")
neighbours = {}
with codecs.open(tens_path + "other/dutchCP_uni.txt", "r", "utf-8") as f:
    for line in f:
        word, otan, otaf, ptan, ptaf = line[:-1].split("\t")
        neighbours[word] = [otan, otaf, ptan, ptaf]

print("Loading Lexicon neighbours")
neighbours_lex = {}
with codecs.open(tz_path + "Docs/neighbour_lexicon.txt", "r", "utf-8") as f:
    for counter, line in enumerate(f, 1):
        word, lev_neb, lev_neb_num, lev_neb_freq, nlev_neb, nlev_neb_num, nlev_neb_freq = line[:-1].split("\t")
        neighbours_lex[word] = [lev_neb_num, lev_neb_freq]

print("Loading CELEX")
celex = {}
with codecs.open(tens_path + "other/DPW3.CD", "r", "utf-8") as f:
    for line in f:
        l_list = line[:-1].split("\\")
        word = l_list[1]
        syls = l_list[4].split("-")
        if syls == [""]:
            celex[word] = ["NA", "NA"]
        else:
            num_syl = len(syls)
            for counter, syl in enumerate(syls, 1):
                if "'" in syl:
                    stress = counter
                    break
            celex[word] = [str(num_syl), str(stress)]

print("Loading COW word frequencies")
cow_uni = {}
with codecs.open(tens_path + "other/cow1.counts", "r", "utf-8") as f:
    for counter, line in enumerate(f, 1):
        wrd, freq = line[:-1].split("\t")
        cow_uni[wrd] = freq
        if counter % 1000000 == 0:
            print(str((float(counter) / float(5052213)) * 100) + " %")

print("Loading COW bigram frequencies")
cow = {}
with codecs.open(tens_path + "other/cow2.counts", "r", "utf-8") as f:
    for counter, line in enumerate(f, 1):
        bigram, freq = line[:-1].split("\t")
        cow[bigram] = freq
        if counter % 1000000 == 0:
            print(str((float(counter) / float(73883219)) * 100) + " %")

print("Loading OOV Conversion Table")
oov_conv = {}
with codecs.open(tens_path + "IFADVcorpus/oov_conv_table_comp-IFADVcorpus.txt", "r", "utf-8") as f:
    for counter, line in enumerate(f, 1):
        if counter > 1:
            c_key, input_w, orig_w, input_i, orig_i, meta_i, meta_l = line[:-1].split("\t")
            oov_conv[c_key] = {
                "input_w": [i for i in input_w.split(",") if i != ""],
                "orig_w": [i for i in orig_w.split(",") if i != ""],
                "input_i": [i for i in input_i.split(",") if i != ""],
                "orig_i": [i for i in orig_i.split(",") if i != ""],
                "meta_i": [i for i in meta_i.split(",") if i != ""],
                "meta_l": [i for i in meta_l.split(",") if i != ""]}

time_words = ["ochtends", "morgens", "middags", "avonds", "nachts", "maandags", "dinsdags", "woensdags", "donderdags", "vrijdags", "zaterdags", "zondags", "weekends", "winters", "zomers", "daags"]

excl_words = ["weegs", "graads", "wils"]

speakers = {}
with codecs.open(tens_path + "IFADVcorpus/speakers.csv", "r", "utf-8") as f:
    for c, l in enumerate(f, 1):
        if c > 1:
            file_f, s1, s2 = l[:-1].split(",")
            speakers[file_f] = {"spreker1": s1, "spreker2": s2}


def createTemp(tg_path):
    with codecs.open(tg_path, "r") as f:
        tg_string = f.read()
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
    ort_path = glob.glob(ifadv_path + "ort/" + fp + "*.ort")[0]
    tg = loadTextGrid(ort_path)
    spreker = tg.getNames()[int(tier) - 1]
    spkr = speakers[fp][spreker]
    return spkr


def checkCanonical(w, position, chunk_id):
    """position should be either 0 (first sub-word) or -1 (final sub-word)"""
    nw = re.sub(r'\*[a-z]', '', re.sub(r"[!?.,:;\t\n\r]*", "", w.lower())).strip("-'")
    nw = "en" if nw == "&" else nw
#    if re.search(r"\*[^ ]*", w):
#        return "NA"
    if nw in kaldi_lex:
        phonlist = kaldi_lex[nw].split(" ")
        return [phonlist, False]
    elif re.sub(r"[!?.,:;\t\n\r]*", "", w) in oov_conv[chunk_id]["orig_w"]:
        print(w, "in OOV table")
        w_i = oov_conv[chunk_id]["orig_w"].index(re.sub(r"[!?.,:;\t\n\r]*", "", w))
        sw = oov_conv[chunk_id]["input_w"][w_i].split(" ")[position]
        if sw in kaldi_lex:
            phonlist = kaldi_lex[sw].split(" ")
            return [phonlist, True]
        else:
            return [None, True]
    else:
        print(w, "not in OOV table")
        return [None, False]


def getAnnotInfo(orig_path, c_start, c_end, spkr, word_index):
    """Calls a Praat script which finds the chunk and returns info on phonetic context."""
    output = subprocess.check_output([tz_path + "praat_nogui", "--run", tz_path + "GitHub/dmc-scripts/getAnnotInfo.praat", orig_path, c_start, c_end, spkr, str(word_index), "IFADVcorpus/kaldi_annot/v2/"]).decode("utf-8")[:-1].split(" ")
#    print(output)
    return output


def getNDLphons(orig_path, c_start, c_end, spkr, word_index, wndw_start, wndw_end):
    """Calls a Praat script which finds the chunk and returns info on phonetic context."""
    output = subprocess.check_output([tz_path + "praat_nogui", "--run", tz_path + "GitHub/dmc-scripts/getNDLphons2.praat", orig_path, c_start, c_end, spkr, str(word_index), str(wndw_start), str(wndw_end), "IFADVcorpus/kaldi_annot/v2/", str(n_phones)]).decode("utf-8")[:-1].split(" ")
#    print(output)
    return output


def getSentenceInfo(rt, spkr, from_t, to_t, wrd):
    candidates = []
#    wrd = "'s" if wrd == "da's" else wrd
    for child in rt:
        if child.tag == "tau":
            if child.attrib["s"] == spkr:
                for word in child:
                    if "{0:.3f}".format(float(word.attrib["tb"])).encode("utf-8") == from_t and "{0:.3f}".format(float(word.attrib["te"])).encode("utf-8") == to_t:
#                        print("{0:.3f}".format(float(word.attrib["tb"])), from_t, "{0:.3f}".format(float(word.attrib["te"])), to_t)
#                        print(wrd.encode("utf-8"), h.unescape(word.attrib["w"]).encode("utf-8"))
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
            if re.search(r"({})$".format("|".join(time_words)), wrd):
                type_of_s = "GEN-TIME"
            else:
                if re.search(r"({})$".format("|".join(excl_words)), wrd):
                    type_of_s = "OTHER"
                else:
                    type_of_s = "GEN-POSS"
        else:
            type_of_s = "S"
    elif word_class == "ADJ":
        if "met-s" in pos_attr:
            type_of_s = "PART"
        elif "dim" in pos_attr:
            type_of_s = "OTHER"  # stilletjes
        else:
            type_of_s = "S"
    elif word_class == "WW":
        type_of_s = "S"
    elif word_class == "TW":
        if "prenom" in pos_attr:
            if "bijz" in pos_attr:
                type_of_s = "OTHER"  # eens geestes zijn
            else:
                type_of_s = "S"
        if "nom" in pos_attr:
            type_of_s = "OTHER"  # met z'n tweetjes
        else:
            type_of_s = "S"
    elif word_class == "VNW":
        if "gen" in pos_attr:
            if "onbep" in pos_attr and "pron" in pos_attr:
                type_of_s = "GEN-POSS"  # iemands
            elif "recip" in pos_attr:
                type_of_s = "GEN-POSS"  # elkaars
            else:
                type_of_s = "OTHER"  # mijns inziens, wiens
        else:
            type_of_s = "S"
    elif word_class == "LID":
        type_of_s = "OTHER"  # des
    elif word_class == "SPEC" or word_class == "LET":
        type_of_s = "NA"
    else:
        type_of_s = "S"
    return w_pos, word_class, type_of_s


def phones2diphones(phone_list):
    diphones = ["".join(phone_list[num - (n_phones - 1):num]) + phon for num, phon in enumerate(phone_list, 0) if num > (n_phones - 2)] if len(phone_list) > (n_phones - 1) else ["".join(phone_list)]
    return diphones


def getNDLinfo(rt, s_index, w_in_sent, orig_path, c_start, c_end, spkr, w_in_chun, final_phon):
    file_name = rt.attrib["ref"]
    window = []
    wndw_start = 100
    sem = "."
    for i in range(w_in_sent + ndl_window_start, w_in_sent + ndl_window_end + 1):
        matches = rt.findall(".//pw[@ref='{}']".format(file_name + "." + str(s_index) + "." + str(i)))
        if len(matches) != 0:
#            print(i)
            if wndw_start == 100:
                wndw_start = i - w_in_sent
            w = matches[0].attrib["w"]
            window.append(sem + w + sem)
            wndw_end = i - w_in_sent
            if i == w_in_sent:
                outcome1 = sem + w + sem
                lem = matches[0].attrib["lem"]
                pos = matches[0].attrib["pos"]
                w_class = re.search(r".+(?=\()", pos).group()
                pos_attr = re.search(r"(?<=\().*(?=\))", pos).group()
                if w_class == "WW":
#                    outcome3 = "NONMORPH" if (pos_attr == "pv,tgw,ev" and final_phon == segment) else ""  # added final_phon constraint to mimick Tomaschek
                    outcome3 = "NONMORPH" if pos_attr == "pv,tgw,ev" else ""
                elif w_class == "N":
                    attr_list = pos_attr.split(",")
                    if "mv" in attr_list:
#                    if "mv" in attr_list and final_phon == segment:  # added final_phon constraint to mimick Tomaschek
                        outcome3 = "PL"
                    else:
#                        outcome3 = "NONMORPH" if (w == lem and final_phon == segment) else ""  # added final_phon constraint to mimick Tomaschek
                        outcome3 = "NONMORPH" if w == lem else ""
                else:
#                    outcome3 = "NONMORPH" if (w == lem and final_phon == segment) else ""  # added final_phon constraint to mimick Tomaschek
                    outcome3 = "NONMORPH" if w == lem else ""
                outcome2 = sem + lem + sem if w != lem else ""
#    print(w_in_chun, wndw_start, wndw_end)
    pre_phons, boundary, post_phons = getNDLphons(orig_path, c_start, c_end, spkr, w_in_chun, wndw_start, wndw_end)
    print(pre_phons, boundary, post_phons)
    if boundary == "":
        boundary_diph = ""
    else:
        if boundary.count("_") < n_phones - 1:
            assert pre_phons == ""
        num_missing = (n_phones - 1) - boundary.count("_")
        boundary_diph = "#" * num_missing + "".join(boundary.split("_"))
    if boundary != "" and wndw_start > ndl_window_start:
        if pre_phons == "":
            pre_phons = "#"
        else:
            pre_phons = "#_" + pre_phons
    if boundary != "" and wndw_end < ndl_window_end:
        if post_phons == "":
            post_phons = "#"
        else:
            post_phons = post_phons + "_#"
    pre_phon_list = pre_phons.split("_")
    boundary_phon_list = boundary.split("_")
    pre_diph = phones2diphones(pre_phon_list + boundary_phon_list[:-1]) if pre_phons != "" else []
    post_diph = phones2diphones(boundary.split("_")[1:] + post_phons.split("_")) if post_phons != "" else []
    return window, pre_diph, boundary_diph, post_diph, outcome1, outcome2, outcome3


def findWord(rt, s_index, w_index, shift):
    file_name = rt.attrib["ref"]
    matches = rt.findall(".//tw[@ref='{}']".format(file_name + "." + str(s_index) + "." + str(w_index + shift)))
    if len(matches) == 0:
        if shift > 0:
            matches = rt.findall(".//tw[@ref='{}']".format(file_name + "." + str(s_index + 1) + "." + str(shift)))
        else:
            prev_sent = rt.findall("./tau[@ref='{}']".format(file_name + "." + str(s_index - 1)))
            if len(prev_sent) > 0:
                matches = prev_sent[0].getchildren()[-1:]
            else:
                matches = []
    shift_word = matches[0].attrib["w"] if len(matches) > 0 else ""
    return shift_word


def getHNR(fp, c, t):
    """Calls praat script for HNR measurement"""
    print("Getting HNR from Praat")
    output = subprocess.check_output([tz_path + "praat_nogui", "--run", tz_path + "GitHub/dmc-scripts/getHarmonicity.praat", fp, str(c), str(t)]).decode("utf-8")[:-1]
    return output


def getOOVmeta(c_id, w_num):
    if w_num in oov_conv[c_id]["meta_i"]:
        i_i = oov_conv[c_id]["meta_i"].index(w_num)
        return oov_conv[c_id]["meta_l"][i_i]
    else:
        return "NA"


def parseLine(f_path, chan, from_time, to_time, ort, tier, new_file):
    global old_speaker
    global hnr
    speaker = getSpeaker(f_path, tier)
    if new_file:
        with codecs.open(ifadv_path + "skp-ort/" + f_path + ".skp", "r", "utf-8") as h:
            skp_txt = h.read()
#        skp_txt = codecs.decode(skp_gz, "ascii")
#        skp_txt = skp_txt.encode("utf-8")
#        for ent in entitydefs:
#            skp_txt = re.sub(r"&{};".format(ent), entitydefs[ent].decode("latin-1"), skp_txt)
#            skp_txt.replace("&" + ent + ";", entitydefs[ent].decode("latin-1"))
#        print(skp_txt)
        global skp_root
        skp_root = ET.fromstring(skp_txt)
#        print(ET.tostring(skp_root))
        with codecs.open(ifadv_path + "tag/" + f_path + ".tag", "r", "utf-8") as h:
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
    # clean up ort
    word_list = [word for word in ort.split(" ") if word not in ["", " "]]
    oov = False
    output_lines = []
    ndl_lines = []
    found = None
    parent = None
    for counter, word in enumerate(word_list, 1):
        # check for segment / oov
        chunk_id = ",".join([f_path, tier, from_time, to_time])
        seg_var, split_up = checkCanonical(word, -1, chunk_id)
        word = re.sub(r'\*[^ ]*', '', re.sub(r"[!?.,:;\t\n\r]*", "", word))
        sent_i, word_sent_i = getSentenceInfo(skp_root, speaker, from_time, to_time, word)
        if not sent_i:
            continue
        old_found = found
        old_parent = parent
        found = skp_root.findall(".//tw[@ref='{}']".format(".".join([f_path.split("/")[-1], str(sent_i), str(word_sent_i)])))[0]
        parent = skp_root.findall("./tau[@ref='{}']".format(".".join([f_path.split("/")[-1], str(sent_i)])))[0]
#        parent.remove(found)
        cue_lexomes, pre_diphones, boundary_diphones, post_diphones, lexome1, lexome2, lexome3 = getNDLinfo(tag_root, sent_i, word_sent_i, f_path, from_time, to_time, speaker, counter, seg_var[-1] if seg_var else None)
        if not seg_var:
            oov = True
        elif seg_var[-1] != segment:
            pass
        else:
            word_chunk_i = counter
            word_phon = " ".join(seg_var)
            num_phon = str(len(seg_var))
            subtlexwf, lg10wf = subtlex[word] if word in subtlex else ["NA", "NA"]
            cow_word = re.sub(r"'", "", word.lower())
            cow_wf = cow_uni[cow_word] if cow_word in cow_uni else "0"
            otan, otaf, ptan, ptaf = neighbours[word] if word in neighbours else ["NA", "NA", "NA", "NA"]
            lex_neb_num, lex_neb_freq = neighbours_lex[word] if word in neighbours_lex else ["NA", "NA"]
#            sent_i, word_sent_i = getSentenceInfo(skp_root, speaker, from_time, to_time, word)
#            found = skp_root.findall(".//tw[@ref='{}']".format(".".join([f_path.split("/")[-1], str(sent_i), str(word_sent_i)])))[0]
#            parent = skp_root.findall("./tau[@ref='{}']".format(".".join([f_path.split("/")[-1], str(sent_i)])))[0]
#            parent.remove(found)
            next_word = findWord(skp_root, sent_i, word_sent_i, 1)
            cow_next_word = re.sub(r"'", "", next_word.lower())
            next_wf = cow_uni[cow_next_word] if cow_next_word in cow_uni else "0"
            bigram = cow_word + " " + cow_next_word
            bigram_f = cow[bigram] if bigram in cow else "0"
            prev_word = findWord(skp_root, sent_i, word_sent_i, -1)
            cow_prev_word = re.sub(r"'", "", prev_word.lower())
            prev_wf = cow_uni[cow_prev_word] if cow_prev_word in cow_uni else "0"
            prev_bigram = cow_prev_word + " " + cow_word
            prev_bigram_f = cow[prev_bigram] if prev_bigram in cow else "0"
            if counter == len(word_list):
                next_phon = "SIL"
            else:
                next_trans = checkCanonical(word_list[counter], 0, chunk_id)[0]
                if next_trans:
                    next_phon = next_trans[0]
                else:
                    next_phon = "NA"
            if len(seg_var) > 1:
                prev_phon = seg_var[-2]
            else:
                if split_up:
                    prev_trans = checkCanonical(word, -2, chunk_id)[0]
                    if prev_trans:
                        prev_phon = prev_trans[-1]
                    else:
                        prev_phon = "NA"
                else:
                    if counter == 1:
                        prev_phon = "SIL"
                    else:
                        prev_trans = checkCanonical(word_list[counter - 2], -1, chunk_id)[0]
                        if prev_trans:
                            prev_phon = prev_trans[-1]
                        else:
                            prev_phon = "NA"
#            oov_meta = getOOVmeta(chunk_id, counter)
            phon_pron, next_phon_pron, prev_phon_pron, overlap, oov_meta = getAnnotInfo(f_path, from_time, to_time, speaker, counter)
            word_pos, word_class, type_of_s = getPOS(tag_root, sent_i, word_sent_i, word)
            num_syl, word_stress = celex[word] if word in celex else ["NA", "NA"]
            # if no syllable info in CELEX or affixed word not in CELEX
            if type_of_s not in ["S", "OTHER", "NA"] and num_syl == "NA" and word_stress == "NA":
                word_stem = re.sub(r"'?s?$", "", word)
                num_syl, word_stress = celex[word_stem] if word_stem in celex else ["NA", "NA"]
            # update NDL outcome
            if type_of_s == "S":
                lexome3 = "NONMORPH"
            elif type_of_s not in ["OTHER", "NA", "GEN-TIME"]:
                lexome3 = re.sub(r'-', "", type_of_s[:])
            else:
                lexome3 = ""
            other_ndl_cues = "_".join(cue_lexomes + pre_diphones + post_diphones)
            output_lines.append([str(word_chunk_i), str(sent_i), str(word_sent_i), word, word_phon, num_phon, phon_pron, prev_phon, prev_phon_pron, next_phon, next_phon_pron, overlap, oov_meta, word_pos, word_class, type_of_s, speaker, subtlexwf, lg10wf, lex_neb_num, lex_neb_freq, ptan, ptaf, cow_wf, next_word, next_wf, bigram_f, prev_word, prev_wf, prev_bigram_f, num_syl, word_stress, boundary_diphones, other_ndl_cues])
            print(word, word_pos, type_of_s)
        if counter > 1:
            if old_found in old_parent:
                old_parent.remove(old_found)
        if boundary_diphones != "":
            ndl_lines.append(["_".join(cue_lexomes + pre_diphones + [boundary_diphones] + post_diphones), "_".join([outcome for outcome in [lexome1, lexome2, lexome3] if outcome != ""])])
        else:
            ndl_lines.append(["_".join(cue_lexomes + pre_diphones + post_diphones), "_".join([outcome for outcome in [lexome1, lexome2, lexome3] if outcome != ""])])
    if len(output_lines) != 0:
        return [",".join([f_path, chan, from_time, to_time, str(oov), tier] + ol) + "\n" for ol in output_lines], [",".join(nl) + "\n" for nl in ndl_lines]
    else:
        return [], [",".join(nl) + "\n" for nl in ndl_lines]


def readWriteMetaData(core_num="", start_line=1, end_line=num_index_lines):
    """Writes file for each process."""
#    if select.select([sys.stdin, ], [], [], 0.0)[0]:
#        print("Reading from Standard Input")
#        f = codecs.getreader('utf-8')(sys.stdin)
#    else:
    print("Reading from file")
    f = codecs.open(input_file_path, "r", "utf-8")
    with codecs.open(tens_path + "IFADVcorpus/all_s" + core_num + ".csv", "w", "utf-8") as g:
        with codecs.open(tens_path + "IFADVcorpus/ndl" + core_num + ".csv", "w", "utf-8") as h:
            output_header = "wav,chan,chunk_start,chunk_end,oov_in_chunk,tier,word_chunk_i,sent_i,word_sent_i,word_ort,word_phon,num_phon,phon_pron,prev_phon,prev_phon_pron,next_phon,next_phon_pron,overlap,oov_meta,word_pos,word_class,type_of_s,speaker,per_mil_wf,log_wf,lex_neb,lex_neb_freq,ptan,ptaf,cow_wf,next_word,next_wf,bigram_f,prev_word,prev_wf,prev_bigram_f,num_syl,word_stress,ndl_boundary_diph,other_ndl_cues\n"
            g.write(output_header)
            ndl_header = "Cues,Outcomes\n"
            h.write(ndl_header)
            old_f_path = ""
            for l_num, line in enumerate(f, 1):
                if l_num >= start_line and l_num <= end_line:
                    print("core " + core_num + ": " + line[:-1].encode("utf-8"))
                    if line.split(",")[2] == "from":
                        continue
                    file_path, channel, f_time, t_time, orth, tg_tier = line[:-1].split(",")
                    new = True if old_f_path != file_path else False
                    write_lines, ndl_lines = parseLine(file_path, channel, f_time, t_time, orth, tg_tier, new)
                    for wl in write_lines:
                        g.write(wl)
                    for wl in ndl_lines:
                        h.write(wl)
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
    with codecs.open(tens_path + "IFADVcorpus/all_s_comb_ifadv_ndl_tri.csv", "w", encoding="utf-8") as g:
        for core in range(num_cores):
            core_n = str(core + 1 + running_cores)
            with codecs.open(tens_path + "IFADVcorpus/all_s" + core_n + ".csv", "r", encoding="utf-8") as f:
                for fln, f_line in enumerate(f, 1):
                    if not (core > 0 and fln == 1):
                        g.write(f_line)
    with codecs.open(tens_path + "IFADVcorpus/ndl_ifadv_tri.csv", "w", encoding="utf-8") as g:
        for core in range(num_cores):
            core_n = str(core + 1 + running_cores)
            with codecs.open(tens_path + "IFADVcorpus/ndl" + core_n + ".csv", "r", encoding="utf-8") as f:
                for fln, f_line in enumerate(f, 1):
                    if not (core > 0 and fln == 1):
                        g.write(f_line)


if __name__ == '__main__':
    multiProcess()
