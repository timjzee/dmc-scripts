# -*- coding: utf-8 -*-

import re
import tempfile
import textgrid  # https://github.com/kylebgorman/textgrid
import sys
import glob
import codecs
from lxml import etree


tens_path = "/Volumes/tensusers/timzee/ECSD/" if sys.platform == "darwin" else "/vol/tensusers/timzee/ECSD/"

tg_folder = "Annotations/ort/"

encoding = "utf-8"

repl_dict = {
    "trema": {"a": "ä", "e": "ë", "i": "ï", "o": "ö", "u": "ü", "y": "ÿ", "A": "Ä", "E": "Ë", "I": "Ï", "O": "Ö", "U": "Ü", "Y": "Ÿ"},
    "aigu": {"a": "á", "c": "ć", "e": "é", "i": "í", "n": "ń", "o": "ó", "s": "ś", "u": "ú", "y": "ý", "z": "ź", "A": "Á", "C": "Ć", "E": "É", "I": "Í", "N": "Ń", "O": "Ó", "S": "Ś", "U": "Ú", "Y": "Ý", "Z": "Ź"},
    "2aigu": {"o": "ő", "u": "ű", "O": "Ő", "U": "Ű"},
    "grave": {"a": "à", "e": "è", "i": "ì", "o": "ò", "u": "ù", "A": "À", "E": "È", "I": "Ì", "O": "Ò", "U": "Ù"},
    "circum": {"a": "â", "e": "ê", "i": "î", "o": "ô", "u": "û", "A": "Â", "E": "Ê", "I": "Î", "O": "Ô", "U": "Û"},
    "tilde": {"a": "ã", "n": "ñ", "o": "õ", "A": "Ã", "N": "Ñ", "O": "Õ"},
    "caron": {"c": "č", "d": "ď", "e": "ě", "g": "ǧ", "n": "ň", "r": "ř", "s": "š", "t": "ť", "z": "ž", "C": "Č", "D": "Ď", "E": "Ě", "G": "Ǧ", "N": "Ň", "R": "Ř", "S": "Š", "T": "Ť", "Z": "Ž"},
    "strike": {"d": "đ", "D": "Đ"},
    "ring": {"a": "å", "u": "ů", "A": "Å", "U": "Ů"},
    "ogonek": {"a": "ą", "e": "ę", "A": "Ą", "E": "Ę"},
    "ash": {"a": "æ", "A": "Æ"},
    "solidus": {"o": "ø", "l": "ł", "O": "Ø", "L": "Ł"},
    "cedille": {"c": "ç", "C": "Ç"},
    "sharp": {"s": "ß"},
    "thorn": {"t": "þ"},
    "dot": {"z": "ż", "Z": "Ż"}
}


filepaths = []
wavpaths = []
for fp in glob.glob(tens_path + tg_folder + "*/*.TextGrid"):
    filepaths.append(fp)
    wp = re.search(r'pp.*(?=\.TextGrid)', fp).group()
    wavpaths.append(wp)


def makeTempFile(fp):
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(open(fp).read())
    tempf.flush()
    return tempf


def replacePraatEscapes(i_ort):
    i_ort = re.sub(r'(\\[aeiouyAEIOUY]")', lambda m: repl_dict["trema"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aceinosuyzACEINOSUYZ]')", lambda m: repl_dict["aigu"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[ouOU]:)", lambda m: repl_dict["2aigu"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aeiouAEIOU]`)", lambda m: repl_dict["grave"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aeiouAEIOU]\^)", lambda m: repl_dict["circum"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[anoANO]~)", lambda m: repl_dict["tilde"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[cdegnrstzCDEGNRSTZ]<)", lambda m: repl_dict["caron"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[dD]-)", lambda m: repl_dict["strike"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aAuU]o)", lambda m: repl_dict["ring"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aAeE];)", lambda m: repl_dict["ogonek"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aA]e)", lambda m: repl_dict["ash"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[oOlL]/)", lambda m: repl_dict["solidus"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[cC],)", lambda m: repl_dict["cedille"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\ss)", lambda m: repl_dict["sharp"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\th)", lambda m: repl_dict["thorn"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[zZ]!)", lambda m: repl_dict["dot"][m.group()[1]], i_ort)
    return i_ort


def getSentence(tier, start_i, sen_i):
    """gets sentence by concatenating intervals until a '.', '?', or '...' is encountered"""
    tier_l = tier.intervals
    int = tier_l[start_i]
    start_t = int.minTime
    f_name = wavpaths[counter]
    speaker_id = tier.name
    sent = etree.Element("tau", ref="{}.{}".format(f_name, sen_i), s=speaker_id, tb=str(start_t))
    sents = [sent]
    word_i = 0
    int_lab = " "
    int_i = start_i - 1
    while (int_lab[-1] not in [".", "?", "!"] and int_i + 1 < len(tier_l)):
        int_i += 1
        int = tier_l[int_i]
        int_lab = int.mark.strip('" \t\n\r')
        int_lab = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', int_lab)
        if len(int_lab) == 0:
            int_lab = " "
#        if "." in int_lab.split(" ")[-1]:  # handle chunks that consist entirely of e.g. "dus ik...heb"
#            int_lab = "."
        start_t = int.minTime
        end_t = int.maxTime
        int_ort = re.sub(r'[ ]+(?=[.,:;?!])', "", int.mark)
        int_ort = re.sub(r'[!?\.,:;](?=[A-Za-z])', lambda m: m.group() + " ", int_ort)
        int_ort = re.sub(r'[.?!]+', '.', int_ort)
        int_ort = re.sub(r'(?<=[A-Za-z])&(?=[A-Za-z])', ' & ', int_ort)
        words = []
        for w in int_ort.split(" "):
            w = re.sub(r'\s', '', w)
            w = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', w)
            if w not in ["", " "]:
                if "." in w:
                    for c, sw in enumerate(w.split("."), 1):
                        if sw not in ["", '"']:
                            if c != len(w.split(".")):
                                words.append(sw + ".")
                            else:
                                words.append(sw)
                else:
                    words.append(w)
#        words = words + [w for w in int_ort.split(" ") if w not in ["", " "]]
        for w_n, w in enumerate(words, 1):
            w_ort = re.sub(r'[".,:;?!\s]', "", re.sub(r"[\\/][\*vVoO-]", "", w))
            w_ort = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', w_ort)
#            w_ort = replacePraatEscapes(w_ort.encode("utf-8"))
            if w_ort != "":
                word_i += 1
#                print(w_ort.decode("utf-8"))
#                print(ord(w_ort.decode("utf-8")[0]))
                word = etree.Element("tw", ref="{}.{}.{}".format(f_name, sen_i, word_i), tb=str(start_t), te=str(end_t), tt="in", tq="man", w=w_ort)  # .decode("utf-8"))
                sents[-1].append(word)
                if w_n != len(words) and ("." in w):
                    # if a sentence delimiter occurs in the middle of a chunk
                    sents[-1].attrib["te"] = str(end_t)
                    sents[-1].attrib["tt"] = "in"
                    sents[-1].attrib["tq"] = "man"
                    sen_i += 1
                    word_i = 0
                    sents.append(etree.Element("tau", ref="{}.{}".format(f_name, sen_i), s=speaker_id, tb=str(start_t)))
    sents[-1].attrib["te"] = str(end_t)
    sents[-1].attrib["tt"] = "in"
    sents[-1].attrib["tq"] = "man"
    return sents, int_i


def make_tag_files(rt):
    file_n = wavpaths[counter]
    pair_n, part_n = file_n.split("_part")
    file_n_lc = pair_n.upper() + "_part" + part_n
    sp1_n, sp2_n = pair_n[2:].split("_")
    with codecs.open(tens_path + "Annotations/pos/" + pair_n.upper() + "/" + file_n_lc + ".pos", "r", "utf-8") as f:
        spreker1 = []
        spreker2 = []
        spreker = 0
        for l in f:
            if re.search(r'speaker id: {}'.format(sp1_n), l):
                spreker = 1
                spreker1.append([])
            if re.search(r'speaker id: {}'.format(sp2_n), l):
                spreker = 2
                spreker2.append([])
            if file_n_lc not in l:
                if spreker == 1:
                    spreker1[-1].append(l[:-1])
                if spreker == 2:
                    spreker2[-1].append(l[:-1])
    ort_spreker1 = rt.findall(".//tau[@s='{}']".format(sp1_n))
    ort_spreker2 = rt.findall(".//tau[@s='{}']".format(sp2_n))
    both_spreker = spreker1 + spreker2
    both_ort_spreker = ort_spreker1 + ort_spreker2
#    for n, i in enumerate(spreker1, 0):
#        print(n, i, etree.tostring(ort_spreker1[n]))
#        print("\n\n")
#        print(i[0].split("\t")[0], ort_spreker1[n][0].get("w"))
#        assert i[0].split("\t")[0] == ort_spreker1[n][0].get("w")
#    print(etree.tostring(ort_spreker2[-1]))
#    print(spreker1[-1])
    print("ort_spreker1: ", len(ort_spreker1), " spreker1: ", len(spreker1))
#    print(etree.tostring(ort_spreker1[0]), spreker1[0])
    assert len(ort_spreker1) == len(spreker1)
    print("ort_spreker2: ", len(ort_spreker2), " spreker2: ", len(spreker2))
    assert len(ort_spreker2) == len(spreker2)
    tag = etree.Element("ptext", ref=file_n)
    for sn in rt:
        s_index = both_ort_spreker.index(sn)
        s_tags = both_spreker[s_index]
        pau = etree.Element("pau", ref=sn.get("ref"), s=sn.get("s"))
        for t_num, tag_l in enumerate(sn, 0):
            t_info = s_tags[t_num].split("\t")
#            print(t_info, etree.tostring(tag_l), t_num)
            assert t_info[0] == tag_l.get("w")
            pw = etree.Element("pw", ref=tag_l.get("ref"), w=tag_l.get("w"), pos=t_info[1], lem=t_info[2])
            pau.append(pw)
        tag.append(pau)
    with codecs.open(tens_path + "Annotations/tag/" + pair_n.upper() + "/" + file_n_lc + ".tag", "w", "utf-8") as f:
        f.write(etree.tostring(tag, encoding="UTF-8", pretty_print=True).decode("utf-8"))


def make_xml_files(fp):
    file_n = wavpaths[counter]
    pair_n, part_n = file_n.split("_part")
    file_n_lc = pair_n.upper() + "_part" + part_n
    sp1_n, sp2_n = pair_n[2:].split("_")
    tg = textgrid.TextGrid()
    with makeTempFile(fp) as tempf:
        tg.read(tempf.name)
    root = etree.Element("ttext", ref=file_n)
    s1_tier = filter(lambda tier: tier.name == sp1_n, tg)[0]
    s1_tier_l = s1_tier.intervals
    s2_tier = filter(lambda tier: tier.name == sp2_n, tg)[0]
    s2_tier_l = s2_tier.intervals
    s1_i = -1
    s2_i = -1
    s1_lab = ""
    s2_lab = ""
    while not (s1_i + 1 == len(s1_tier_l) and s2_i + 1 == len(s2_tier_l) and s1_lab == "" and s2_lab == ""):
        while s1_lab == "":
            # it is assumed getSentence returns an s1_i which is on the final chunk of a sentence
            if s1_i + 1 == len(s1_tier_l):
                s1_mintime = s1_tier.maxTime
#                print("EINDE S1", "s1_mintime: ", s1_mintime, " s2_mintime: ", s2_mintime)
                if s1_lab == "":
                    break
            else:
                s1_i += 1
                s1_int = s1_tier_l[s1_i]
                s1_lab = s1_int.mark
                s1_lab = re.sub(r'[\s\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', s1_lab)
                s1_mintime = s1_int.minTime
        while s2_lab == "":
            # it is assumed getSentence returns an s2_i which is on the final chunk of a sentence
            if s2_i + 1 == len(s2_tier_l):
                s2_mintime = s2_tier.maxTime
                if s2_lab == "":
                    break
            else:
                s2_i += 1
                s2_int = s2_tier_l[s2_i]
                s2_lab = s2_int.mark
                s2_lab = re.sub(r'[\s\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', s2_lab)
                s2_mintime = s2_int.minTime
        if s1_mintime < s2_mintime:
            # turn = "spreker1"
            sentences, s1_i = getSentence(s1_tier, s1_i, len(root) + 1)
#            print("S1", s1_lab, s1_i, "/", len(s1_tier_l), s2_lab, s2_i, "/", len(s2_tier_l))
            s1_lab = ""
        elif s1_mintime > s2_mintime:
            # turn = "spreker2"
#            print("S2", s1_lab, s1_i, "/", len(s1_tier_l), s2_lab, s2_i, "/", len(s2_tier_l))
            sentences, s2_i = getSentence(s2_tier, s2_i, len(root) + 1)
            s2_lab = ""
        else:
            if s1_mintime == s2_mintime == s1_tier.maxTime:
                break
            else:
                # turn = "spreker1"
                sentences, s1_i = getSentence(s1_tier, s1_i, len(root) + 1)
                s1_lab = ""
        for s in sentences:
            root.append(s)
    with codecs.open(tens_path + "Annotations/skp-ort/" + pair_n.upper() + "/" + file_n_lc + ".skp", "w", "utf-8") as f:
        f.write(etree.tostring(root, encoding="UTF-8", pretty_print=True).decode("utf-8"))
    make_tag_files(root)


textgrid.textgrid.detectEncoding = lambda f: encoding
for counter, filepath in enumerate(filepaths, 0):
    print(filepath)
    make_xml_files(filepath)
