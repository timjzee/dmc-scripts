#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import sys
import re
import tempfile
import textgrid
import codecs
from lxml import etree as ET
from HTMLParser import HTMLParser
from htmlentitydefs import entitydefs
import multiprocessing
import glob

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
ifadv_path = "/Volumes/tensusers/timzee/IFADVcorpus/Annotations/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFADVcorpus/Annotations/"
tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

h = HTMLParser()
# get rid of predefined XML entities; these are handled by XML parser
del entitydefs["amp"]
del entitydefs["quot"]
del entitydefs["lt"]
del entitydefs["gt"]

input_file_path = tens_path + "IFADVcorpus/ifadv_s_ndl_static_final.csv"
# input_file_path = tens_path + "IFADVcorpus/test.csv"
with codecs.open(input_file_path, "r", "utf-8") as f:
    input_file = f.readlines()

running_cores = 0

# Do not run with more than 5 cores.
num_cores = 1
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

speakers = {}
with codecs.open(tens_path + "IFADVcorpus/speakers.csv", "r", "utf-8") as f:
    for c, l in enumerate(f, 1):
        if c > 1:
            file_f, s1, s2 = l[:-1].split(",")
            speakers[file_f] = {"spreker1": s1, "spreker2": s2}


print("Loading CELEX")
celex_morph = {}
with codecs.open(tens_path + "other/DML.CD", "r", "utf-8") as f:
    for line in f:
        l_list = line[:-1].split("\\")
        celex_id = l_list[0]
        morph_tag = l_list[12]
        if morph_tag != "":
            morph_tag2 = re.search(r'(?<=\[)[^\)]*(?=\]$)', morph_tag).group()
            celex_morph[celex_id] = morph_tag2

celex_phon = {}
with codecs.open(tens_path + "other/DPL.CD", "r", "utf-8") as f:
    for line in f:
        l_list = line[:-1].split("\\")
        celex_id = l_list[0]
        cel_word = l_list[1]
        phonemes = l_list[9]
        if celex_id in celex_morph:
            cel_morph = celex_morph[celex_id]
            if cel_word in celex_phon:
                celex_phon[cel_word][cel_morph] = phonemes
            else:
                celex_phon[cel_word] = {}
                celex_phon[cel_word][cel_morph] = phonemes

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

print("Loading SUBTLEX")
subtlex = {}
with codecs.open(tens_path + "other/SUBTLEX-NL.txt", "r", "utf-8") as f:
    for l_num, line in enumerate(f, 1):
        if l_num > 1:
            l_list = line[:-1].split("\t")
            word = l_list[0]
            freq_count = float(l_list[1])
            freq_lemma = float(l_list[5])
            subtlex[word] = freq_count / freq_lemma

print("Loading SUBTLEX2")
subtlex2 = {}
with codecs.open(tens_path + "other/SUBTLEX-NL.with-pos.txt", "r", "utf-8") as f:
    for l_num, line in enumerate(f, 1):
        if l_num > 1:
            l_list = line[:-1].split("\t")
            word = l_list[0]
            pos = [p for p in l_list[14].split(".") if p != ""]
            pos_freqs = [int(p) for p in l_list[15].split(".") if p != ""]
            lem_freqs = [int(p) for p in l_list[16].split(".") if p != ""]
            assert len(pos) == len(pos_freqs)
            if "N" in pos:
                subtlex2[word] = {pos_lab: (pos_freqs[pos_i], lem_freqs[pos_i]) for pos_i, pos_lab in enumerate(pos, 0)}

long_vowels = ["ee", "uu", "oo", "aa"]
# long_diph = ["ei", "eu", "ui", "ou", "oe", "au", "ij", "ooi", "aai"]
# short_vowels = ["e", "u", "i", "o", "a", "y"]
# vowels = long_vowels + long_diph + short_vowels
consonants = ["q", "w", "r", "t", "p", "s", "d", "f", "g", "h", "j", "k", "l", "z", "x", "c", "v", "b", "n", "m"]


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
        tg.read(tempf.name, encoding="utf-8")
    return tg


def getSpeaker(fp, tier):
    ort_path = glob.glob(ifadv_path + "ort2/" + fp + "*.ort")[0]
    tg = loadTextGrid(ort_path)
    spreker = tg.getNames()[int(tier) - 1]
    spkr = speakers[fp][spreker]
    return spkr


def getSentenceInfo(rt, spkr, from_t, to_t, wrd):
    candidates = []
#    wrd = "'s" if wrd == "da's" else wrd
    for child in rt:
        if child.tag == "tau":
            if child.attrib["s"] == spkr:
                for word in child:
#                    print("{0:.3f}".format(float(word.attrib["tb"])).encode("utf-8"), from_t)
                    if "{0:.3f}".format(float(word.attrib["tb"])).encode("utf-8") == from_t and "{0:.3f}".format(float(word.attrib["te"])).encode("utf-8") == to_t:
#                        print("{0:.3f}".format(float(word.attrib["tb"])), from_t, "{0:.3f}".format(float(word.attrib["te"])), to_t)
#                        print(wrd.encode("utf-8"), h.unescape(word.attrib["w"]).encode("utf-8"))
                        if h.unescape(word.attrib["w"]) == wrd:
                            candidates.append((int(word.attrib["ref"].split(".")[1]), int(word.attrib["ref"].split(".")[2])))
    # in case later sentence is positioned above earlier sentence in xml
    match = sorted(candidates)[0] if len(candidates) > 0 else (None, None)
    return match


def getSubWrd(w, chunk_id):
    if chunk_id in oov_conv:
        if re.sub(r"[!?.,:;\t\n\r]*", "", w) in oov_conv[chunk_id]["orig_w"]:
            w_i = oov_conv[chunk_id]["orig_w"].index(re.sub(r"[!?.,:;\t\n\r]*", "", w))
            sw = oov_conv[chunk_id]["input_w"][w_i].split(" ")[-1]
            return sw
        else:
            return None
    else:
        return None


def getVoice(item, c_tag, ch_i):
    if item in celex_phon:
        if c_tag in celex_phon[item]:
            if len(celex_phon[item][c_tag]) > 0:
                final_phoneme = celex_phon[item][c_tag][-1]
                if final_phoneme in ["s", "z"]:
                    return "voiceless" if final_phoneme == "s" else "voiced"
                else:
                    return "voiceless"
            else:
                return "NA"
        else:
            return "NA"
    else:
        subw = getSubWrd(item, ch_i)
        if subw in celex_phon:
            return getVoice(subw, c_tag, ch_i)
        else:
            return "NA"


def getPOS(rt, s_ind, w_ind, wrd, chunk_i, word_class, s_type, w_ph):
    pw_list = rt.findall(".//pw[@ref='{}']".format(".".join([rt.attrib["ref"], str(s_ind), str(w_ind)])))
    # if list is empty --> cross-referencing mistake in CGN; e.g. fv701108.12.8 in .skp = fv701108.13.8 in .tag
    if len(pw_list) == 0:
        print("CROSS-REFERENCE DOES NOT EXIST IN .TAG")
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"], "NA", "NA"
    pw = pw_list[0]
    # we check for wordform, in case cross-reference does exist but is for another word due to CGN mistake
    if not h.unescape(pw.attrib["w"]) in wrd:
        print("WORDFORM MISMATCH IN CROSS-REFERENCE")
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"], "NA", "NA"
    # ignore words that are part of multi-word-units
    if pw.attrib["mwu"] == "1":
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"], "NA", "NA"
    # Get Voice feature
#    w_pos = re.sub(",", ";", pw.attrib["pos"])
#    word_class = re.search(r".+(?=\()", w_pos).group()
    w_lem = unicode(pw.attrib["lem"])
    if s_type == "S":
        if wrd[-1] == "x" or wrd[-2:] in ["'s", "ch", "ts", "ks", "ps"]:
            voicing_feat = "voiceless"
        elif re.search(r'loos$', wrd):
            voicing_feat = "voiced"
        else:
            if word_class == "WW":
#                w_lem = unicode(pw.attrib["lem"])
#                print(w_lem, pw.attrib["pos"][0])
                if pw.attrib["pos"][:2] == "WW":
                    if wrd in ["is", "was"]:
                        voicing_feat = "voiceless"
                    elif w_lem[-3:] == "zen":
                        voicing_feat = "voiced"
                    elif w_lem[-3:] == "sen":
                        voicing_feat = "voiceless"
                    else:
                        voicing_feat = getVoice(w_lem, "V", chunk_i)
                else:
                    voicing_feat = getVoice(w_lem, "V", chunk_i)
            elif word_class in ["N", "ADJ"]:
                if word_class == "N" and (w_ph.split(" ")[-2] in ["I", "E", "A", "U", "O", "@"]):
                    voicing_feat = "voiceless"
                else:
                    w_lem = unicode(pw.attrib["lem"])
                    cel_pos = word_class[0]
                    if wrd == w_lem:
                        voicing_feat = getVoice(wrd, cel_pos, chunk_i)
                    else:
                        voicing_feat = "voiceless"
            else:
                voicing_feat = "voiceless"
    else:
        voicing_feat = "voiceless"
    # Get dependency grammar features
    pau = rt.findall(".//pau[@ref='{}']".format(".".join([rt.attrib["ref"], str(s_ind)])))[0]
    pau_children = list(pau)
    pw_index = pau_children.index(pw)
    # get closest incoming and outgoing link to the right
    dep_index_out = int(pw.attrib["depindex"])
    dep_out = pw.attrib["dep"]
    # get phrase chunks
#    out_phrase1 = pw.attrib["chunker"]
#    out_phrase2 = "NONE"
#    for ch in pau_children:
#        if int(ch.attrib["origindex"]) == dep_index_out:
#            out_phrase2 = ch.attrib["chunker"]
#    out_tup = (dep_index_out, dep_out, True, out_phrase1, out_phrase2)
    out_tup = (dep_index_out, dep_out, True)
    origindex = pw.attrib["origindex"]
    dep_index_in = 0
    dep_in = ""
#    in_phrase1 = ""
    for pw_right in pau_children[pw_index:]:
        if pw_right.attrib["depindex"] == origindex:
            dep_index_in = int(pw_right.attrib["origindex"])
            dep_in = pw_right.attrib["dep"]
#            in_phrase1 = pw_right.attrib["chunker"]
            break
#    in_tup = (dep_index_in, dep_in, False, in_phrase1, out_phrase1)
    in_tup = (dep_index_in, dep_in, False)
    if dep_index_in and dep_index_out > int(origindex):  # if both incoming and outgoing right-adjacent links exist
        f1_index, f1, outgoing = min(out_tup, in_tup)
#        f1_index, f1, outgoing, phrase1, phrase2 = min(out_tup, in_tup)
    else:
        if dep_index_in:  # only incoming
            f1 = dep_in
            f1_index = dep_index_in
            outgoing = False
#            phrase1 = in_phrase1
#            phrase2 = out_phrase1
        elif dep_index_out > int(origindex):  # only outgoing
            f1 = dep_out
            f1_index = dep_index_out
            outgoing = True
#            phrase1 = out_phrase1
#            phrase2 = out_phrase2
        else:
            f1 = "end"
            f1_index = int(origindex) + 1
            outgoing = True
#            phrase1 = out_phrase1
#            phrase2 = "end"
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
#    return [str(feat) for feat in [f1, f2, f3, f4, f5, f6, f7, f8, phrase1, phrase2]]
    return [str(feat) for feat in [f1, f2, f3, f4, f5, f6, f7, f8]], voicing_feat, w_lem


def parseLine(f_path, from_time, to_time, tier, new_file, word, w_cl, type_of_s, word_phons, word_stress):
    global old_speaker
    global hnr
    speaker = getSpeaker(f_path, tier)
    if new_file:
        with codecs.open(ifadv_path + "skp-ort2/" + f_path + ".skp", "r", "utf-8") as h:
            skp_txt = h.read()
#        skp_txt = codecs.decode(skp_gz, "ascii")
#        skp_txt = skp_txt.encode("utf-8")
#        for ent in entitydefs:
#            skp_txt = re.sub(r"&{};".format(ent), entitydefs[ent].decode("latin-1"), skp_txt)
#            skp_txt.replace("&" + ent + ";", entitydefs[ent].decode("latin-1"))
#        print(skp_txt)
        global skp_root
        skp_root = ET.fromstring(skp_txt)
        with codecs.open(ifadv_path + "tag2/" + f_path + ".tag", "r", "utf-8") as h:
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
    rel_f1 = str(subtlex[word]) if word in subtlex else "NA"
    rel_f2 = "1" if type_of_s == "S" else rel_f1
    rel_freq = [rel_f1, rel_f2]
    if re.search(r"[ie]es$", word):
        if word_stress:
            en_word = re.sub(r"s$", "ën", word.encode("utf-8"))
        else:
            en_word = re.sub(r"es$", "ën", word.encode("utf-8"))
    elif re.search(r"(" + "|".join(consonants) + ")es$", word):  # turn words like ziektes into ziekten
        en_word = re.sub(r"s$", "n", word)
    elif re.search("(" + "|".join(consonants) + ")[euioay][rtpdfgklbnm]s$", word) and word_stress:  # turn words like hotel into hotellen
        en_word = re.sub(r"[rtpdfgklbnm]s$", lambda m: m.group()[0] * 2 + "en", word)               # but do not turn appel into appellen
    elif re.search("(" + "|".join(long_vowels) + ")(" + "|".join(consonants) + ")s$", word):        # turn words like uurs into uren
        en_word = re.sub("(" + "|".join(long_vowels) + ")(" + "|".join(consonants) + ")s$", lambda m: m.group()[0] + m.group()[2] + "en", word)
    else:
        en_word = re.sub(r"'*s$", "en", word)
    s_freq, lem_freq = subtlex2[word]["N"] if word in subtlex2 else (0, 0)
    if en_word in subtlex2 and word in subtlex2:
        if subtlex2[en_word]["N"][1] == subtlex2[word]["N"][1]:
            en_freq = subtlex2[en_word]["N"][0]
        elif word in ["hersens", "idees"]:
            en_freq = subtlex2[en_word]["N"][0]
            lem_freq += subtlex2[en_word]["N"][1]
        else:
            en_freq = 0
    else:
        en_freq = 0
#    en_freq = subtlex2[en_word]["N"] if (en_word in subtlex2) else 0
    rel_freq_pl = str(float(s_freq + en_freq) / float(lem_freq)) if lem_freq > 0 else "NA"
    pl_prop = str(float(s_freq) / float(s_freq + en_freq)) if (s_freq + en_freq > 0) else "NA"
    sent_i, word_sent_i = getSentenceInfo(skp_root, speaker, from_time, to_time, word)
    syntax_feats, voice_feat, lem = getPOS(tag_root, sent_i, word_sent_i, word, ",".join([f_path, tier, from_time, to_time]), w_cl, type_of_s, word_phons)
    return syntax_feats, voice_feat, lem, rel_freq, str(s_freq), str(en_freq), pl_prop, str(lem_freq), rel_freq_pl


def readWriteMetaData(core_num="", start_line=1, end_line=num_index_lines):
    """Writes file for each process."""
#    if select.select([sys.stdin, ], [], [], 0.0)[0]:
#        print("Reading from Standard Input")
#        f = codecs.getreader('utf-8')(sys.stdin)
#    else:
    print("Reading from file")
    f = codecs.open(input_file_path, "r", "utf-8")
    with codecs.open(tens_path + "IFADVcorpus/syntax_s" + core_num + ".csv", "w", "utf-8") as g:
        output_header = "wav,chan,chunk_start,chunk_end,tier,word_chunk_i,sent_i,word_sent_i,word_ort,word_phon,num_phon,phon_pron,prev_phon,prev_phon_pron,next_phon,next_phon_pron,word_pos,word_class,type_of_s,speaker,per_mil_wf,log_wf,lex_neb,lex_neb_freq,ptan,ptaf,cow_wf,next_word,next_wf,bigram_f,prev_word,prev_wf,prev_bigram_f,num_syl,word_stress,ndl_boundary_diph,other_ndl_cues,s_dur,kal_start,kal_end,s_cog_full,s_cog_window,proportion_voiced,proportion_voiced2,mean_hnr,speech_rate_pron,base_dur,num_syl_pron,num_cons_pron,speaker_sex,birth_year,next_phon_dur,prev_phon_dur,prev_mention,phrase_final,nn_start,nn_end,nn_start_score,nn_end_score,syntax_f1,syntax_f2,syntax_f3,syntax_f4,syntax_f5,syntax_f6,syntax_f7,syntax_f8,underlying_voice,lemma,rel_freq1,rel_freq2,s_freq,en_freq,pl_prop,lem_freq,rel_freq_pl\n"
        g.write(output_header)
        old_f_path = ""
        for l_num, line in enumerate(f, 1):
            if l_num >= start_line and l_num <= end_line:
                line_l = line[:-1].split(",")
                if line_l[1] == "chan":
                    continue
                file_path = line_l[0]
                c_start, c_end, tg_tier = line_l[2:5]
                w_phons = line_l[9]
                word_cl = line_l[17]
                type_s = line_l[18]
                if type_s != "PL":
                    g.write(line[:-1] + ",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA\n")
                    continue
#                print(line_l)
                print("core " + core_num + ": " + line[:-1].encode("utf-8"))
                num_syl = line_l[33]
                word_stress = line_l[34]
                stressed = num_syl == word_stress
                c_start = "{0:.3f}".format(float(c_start))
                c_end = "{0:.3f}".format(float(c_end))
                w = line_l[8]
                new = True if old_f_path != file_path else False
                syn_feats, voice, lemma, relfreq, pl_s_freq, pl_en_freq, pl_propo, lemma_freq, relfreq_pl = parseLine(file_path, c_start, c_end, tg_tier, new, w, word_cl, type_s, w_phons, stressed)
                g.write(",".join(line_l + syn_feats + [voice, lemma] + relfreq + [pl_s_freq, pl_en_freq, pl_propo, lemma_freq, relfreq_pl]) + "\n")
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
    with codecs.open(tens_path + "IFADVcorpus/synvoirelPL_s_comb_ifadv.csv", "w", encoding="utf-8") as g:
        for core in range(num_cores):
            core_n = str(core + 1 + running_cores)
            with codecs.open(tens_path + "IFADVcorpus/syntax_s" + core_n + ".csv", "r", encoding="utf-8") as f:
                for fln, f_line in enumerate(f, 1):
                    if not (core > 0 and fln == 1):
                        g.write(f_line)


if __name__ == '__main__':
    multiProcess()
