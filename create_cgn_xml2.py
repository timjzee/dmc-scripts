# -*- coding: utf-8 -*-

import re
import textgrid  # https://github.com/kylebgorman/textgrid
import sys
import glob
import codecs
from lxml import etree

tens_path = "/Volumes/tensusers/timzee/cgn/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/"

component = "c"

tg_folder = "Annotations/ort/comp-" + component + "/"

cgn_path = "/vol/bigdata2/corpora2/CGN2/data/annot/"

language = "v"

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


wavpaths = []
filepaths = []
for fp in glob.glob(tens_path + tg_folder + "f" + language + "*.ort"):
    filepaths.append(fp)
    wavpaths.append(fp.split("/")[-1].split(".")[0])

# filepaths = ["/vol/tensusers/timzee/cgn/Annotations/ort/comp-d/fv700205.ort"]
# wavpaths = ["fv700205"]

allowed_speakers = []
with open(tens_path + "speakers.txt", "r") as f:
    for num, line in enumerate(f, 1):
        if num > 1:
            allowed_speakers.append(line.split("\t")[4])


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
#    print(start_i, sen_i)
    tier_l = tier.intervals
    int = tier_l[start_i]
    start_t = int.minTime
    f_name = wavpaths[counter]
    speaker_id = tier.name
    sent = etree.Element("tau", ref="{}.{}".format(f_name, sen_i), s=speaker_id, tb="{:.3f}".format(start_t))
    sents = [sent]
    word_i = 0
    int_lab = " "
    int_i = start_i - 1
    while (int_lab[-1] not in [".", "?"] and int_i + 1 < len(tier_l)):
        int_i += 1
        int = tier_l[int_i]
        int_lab = int.mark.strip(" ") if len(int.mark.strip(" ")) > 0 else " "
        int_lab = re.sub(r'(?<=[A-Za-z])\.\.\.(?=[A-Za-z])', ' ', int_lab)  # replace '...' with ' ' in first instance in 'heeft denk ik...wel...'
        int_lab = re.sub(r'(?<=[A-Za-z])\.\.(?=[A-Za-z])', '. ', int_lab)  # replace '..' with '. ' in 'ja ik was..ja ja'
        int_lab = re.sub(r'(?<=[A-Za-z])[\.?](?=[A-Za-z])', lambda m: m.group() + ' ', int_lab)  # replace '.' with '. ' in 'ggg.uh n*a'
        if "." in int_lab.split(" ")[-1]:  # handle chunks that consist entirely of e.g. "dus ik...heb"
            int_lab = "."
        start_t = int.minTime
        end_t = int.maxTime
        int_ort = re.sub(r' (?=[.,:;?!])', "", int.mark)
        int_ort = re.sub(r'^\.', '', int_ort)  # handle chunks that mistakenly start in '.'
        int_ort = re.sub(r'(?<=[A-Za-z])\.\.\.(?=[A-Za-z])', ' ', int_ort)  # replace '...' with ' ' in first instance in 'heeft denk ik...wel...'
        int_ort = re.sub(r'(?<=[A-Za-z])\.\.(?=[A-Za-z])', '. ', int_ort)  # replace '..' with '. ' in 'ja ik was..ja ja'
        int_ort = re.sub(r'(?<=[A-Za-z])[\.?](?=[A-Za-z])', lambda m: m.group() + ' ', int_ort)  # replace '.' with '. ' in 'ggg.uh n*a'
#        int_ort = re.sub(r'\.(?=[A-Za-z])', '. ', int_ort)  # separate chunks that consist entirely of e.g. "dus ik...heb"
        words = [w for w in int_ort.split(" ") if w not in ["", " "]]
        for w_n, w in enumerate(words, 1):
            w_ort = re.sub(r"[.,?\t\n\r]", "", re.sub(r"\*[^ ]*", "", w))
            w_ort = replacePraatEscapes(w_ort.encode("utf-8"))
            if w_ort != "":
                word_i += 1
                word = etree.Element("tw", ref="{}.{}.{}".format(f_name, sen_i, word_i), tb="{:.3f}".format(start_t), te="{:.3f}".format(end_t), tt="in", tq="man", w=w_ort.decode("utf-8"))
                sents[-1].append(word)
                if w_n != len(words) and ("." in w or "?" in w):  # handles mistakes like waren...nou the same as POS file
                    # if a sentence delimiter occurs in the middle of a chunk
                    sents[-1].attrib["te"] = str(end_t)
                    sents[-1].attrib["tt"] = "in"
                    sents[-1].attrib["tq"] = "man"
                    sen_i += 1
                    word_i = 0
                    sents.append(etree.Element("tau", ref="{}.{}".format(f_name, sen_i), s=speaker_id, tb="{:.3f}".format(start_t)))
    sents[-1].attrib["te"] = str(end_t)
    sents[-1].attrib["tt"] = "in"
    sents[-1].attrib["tq"] = "man"
    return sents, int_i


def make_tag_files(rt, spkrs):
    file_n = wavpaths[counter]
    text_dict = {}
    for spkr in spkrs:
        text_dict[spkr] = {"pos": [], "ort": []}
        text_dict[spkr]["ort"] = rt.findall(".//tau[@s='{}']".format(spkr))
    with codecs.open(tens_path + "Annotations/pos/comp-" + component + "/" + file_n + ".pos", "r", "utf-8") as f:
        for l in f:
            if file_n in l:
                for spkr in spkrs:
                    if spkr in l:
                        text_dict[spkr]["pos"].append([])
                        break
            else:
                text_dict[spkr]["pos"][-1].append(l[:-1])

    all_spreker = []
    all_ort_spreker = []
#    print(spkrs)
    for spkr in spkrs:
        all_spreker.extend(text_dict[spkr]["pos"])
        all_ort_spreker.extend(text_dict[spkr]["ort"])
        print(spkr, "ort length:", len(text_dict[spkr]["ort"]), " pos length:", len(text_dict[spkr]["pos"]))
#        for n, i in enumerate(text_dict[spkr]["pos"], 0):
#            print(n, i, etree.tostring(text_dict[spkr]["ort"][n]))
#            print("\n\n")
#            print(i[0].split("\t")[0], text_dict[spkr]["ort"][n][0].get("w"))
#            assert i[0].split("\t")[0][0] in text_dict[spkr]["ort"][n][0].get("w")
#        print(etree.tostring(text_dict[spkr]["ort"][-1]))
        assert len(text_dict[spkr]["ort"]) == len(text_dict[spkr]["pos"])
    tag = etree.Element("ptext", ref=file_n)
    for sn in rt:
#        print(sn.attrib["s"])
        if sn.tag == "tau" and sn.attrib["s"] in spkrs:
            s_index = all_ort_spreker.index(sn)
            s_tags = all_spreker[s_index]
            s_tags = [i for i in s_tags if (i.split("\t")[1] != "LET()" or i.split("\t")[0] == "&" or re.search("^'[A-Za-z]+", i.split("\t")[0]))]  # we want to skip all interpunction except for &
            pau = etree.Element("pau", ref=sn.get("ref"), s=sn.get("s"))
#            print(s_tags)
            for t_num, tag_l in enumerate(sn, 0):
#                print(etree.tostring(tag_l), t_num)
                t_info = s_tags[t_num].split("\t")
    #            print(t_info, etree.tostring(tag_l), t_num)
                assert t_info[0][0], tag_l.get("w")
    #            assert re.search('.*'.join([c for c in t_info[0]]), tag_l.get("w"))
    #            assert t_info[0] in tag_l.get("w")
                pw = etree.Element("pw", ref=tag_l.get("ref"), w=tag_l.get("w"), pos=t_info[1], lem=t_info[2], dep=t_info[4], depindex=t_info[5], origindex=t_info[6], mwu=t_info[7])
                pau.append(pw)
            tag.append(pau)
    with codecs.open(tens_path + "Annotations/tag/comp-" + component + "/" + wavpaths[counter] + ".tag", "w", "utf-8") as f:
        f.write(etree.tostring(tag, encoding="UTF-8", pretty_print=True).decode("utf-8"))


def make_xml_files(fp):
    tg = textgrid.TextGrid()
    tg.read(fp)
    global skp_root
    skp_root = etree.Element("ttext", ref=wavpaths[counter])
    speakers = []
    for s1_tier in tg.tiers:
        if s1_tier.name in allowed_speakers:
            speakers.append(s1_tier.name)
            s1_tier_l = s1_tier.intervals
            s1_i = -1
            while not s1_i + 1 >= len(s1_tier_l):
                s1_i += 1
                sentences, s1_i = getSentence(s1_tier, s1_i, len(skp_root) + 1)
                for ns, s in enumerate(sentences, 1):
                    if s1_i + 1 == len(s1_tier_l) and ns == len(sentences):
                        if s1_tier_l[-1].mark.strip(" ") == "":
                            break
                    skp_root.append(s)
    with codecs.open(tens_path + "Annotations/skp-ort/comp-" + component + "/" + wavpaths[counter] + ".skp", "w", "utf-8") as f:
        f.write(etree.tostring(skp_root, encoding="UTF-8", pretty_print=True).decode("utf-8"))
    make_tag_files(skp_root, speakers)


#textgrid.textgrid.detectEncoding = lambda f: encoding
for counter, filepath in enumerate(filepaths, 0):
    print(filepath)
    make_xml_files(filepath)
