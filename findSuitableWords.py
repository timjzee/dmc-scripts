import re
import sys
import math

tens_path = "/Volumes/tensusers/timzee/other/" if sys.platform == "darwin" else "/vol/tensusers/timzee/other/"

verbal_stem_infs = {}
with open(tens_path + "suitable_verbal_stems2.txt", "r") as f:
    for l in f:
        verbal_stem_infs[l[:-1].split(",")[0]] = {"subtlex_l_freq": l[:-1].split(",")[-1], "subtlex_f_freq": l[:-1].split(",")[2]}

celex_vs = {}
celex = {}
with open(tens_path + "DML.CD", "r") as f:
    for l in f:
        l_id, wrd, lem_freq = l.split("\\")[:3]
        if re.search(r'^[0-9]+\\[a-z]+[^(en)io]en\\.*[a-z]\\.*\)\[V\]\\', l):
            celex[l_id] = {"inf_ort": wrd, "celex_l_freq": lem_freq}
        elif wrd in verbal_stem_infs:
            celex_vs[l_id] = {"inf_ort": wrd, "celex_l_freq": lem_freq}

f_id_list_vs = []
f_id_list = []
with open(tens_path + "DMW.CD", "r") as f:
    for l in f:
        l_list = l[:-1].split("\\")
        f_id = l_list[0]
        l_id = l_list[3]
        m_type = l_list[4]
        if l_id in celex and m_type == "tm":
            f_id_list.append(f_id)
        elif l_id in celex_vs and m_type == "te1":
            f_id_list_vs.append(f_id)

celex2_vs = {}
celex2 = {}
with open(tens_path + "DPW.CD", "r") as f:
    for l in f:
        f_id, wrd, f_freq, l_id, phon, syl_struc = l.split("\\")[:6]
        if f_id in f_id_list and l_id in celex:
            syls = [i for i in phon.split("-") if i != ""]
            num_syl = len(syls)
            counter = 0
            for syl in syls:
                counter += 1
                if "'" in syl:
                    break
            stress = counter
            celex2[wrd] = {"subtlex_l_freq": 0, "subtlex_f_freq": 0, "celex_l_freq": celex[l_id]["celex_l_freq"], "celex_f_freq": f_freq, "n_syl": num_syl, "stress": stress, "syl_struc": syl_struc}
        elif f_id in f_id_list_vs and l_id in celex_vs:
            syls = [i for i in phon.split("-") if i != ""]
            num_syl = len(syls)
            counter = 0
            for syl in syls:
                counter += 1
                if "'" in syl:
                    break
            stress = counter
            celex2_vs[wrd] = {"subtlex_l_freq": verbal_stem_infs[wrd + "en"]["subtlex_l_freq"], "subtlex_f_freq": verbal_stem_infs[wrd + "en"]["subtlex_f_freq"], "celex_l_freq": celex_vs[l_id]["celex_l_freq"], "celex_f_freq": f_freq, "n_syl": num_syl, "stress": stress, "syl_struc": syl_struc}

with open(tens_path + "SUBTLEX-NL.master.txt", "r") as f:
    for l in f:
        l_list = l[:-1].split("\t")
        if l_list[0] in celex2 and l_list[1] == "WW":
            celex2[l_list[0]]["subtlex_l_freq"] += int(l_list[4])
        elif l_list[2] in celex2 and l_list[3].split(",")[0] in ["inf", "pv"]:
            celex2[l_list[2]]["subtlex_f_freq"] += int(l_list[4])

# note that celex_f_freq in celex2_vs represents different inflections than celex_f_freq in celex2
# find matches in terms of n_syl, same length of vowels, stress, frequencies
syl_matches = {}
for i in celex2_vs:
    l_freq = float(celex2_vs[i]['subtlex_l_freq'])
    log_l_freq = math.log10(l_freq)
    f_freq = float(celex2_vs[i]['subtlex_f_freq'])
    f_prop = f_freq / l_freq
    syl_matches[i] = []
    for j in celex2:
        if celex2_vs[i]["stress"] == celex2[j]["stress"] and "".join(celex2_vs[i]["syl_struc"].split("C")) == "".join(celex2[j]["syl_struc"].split("C")):
            m_l_freq = float(celex2[j]['subtlex_l_freq'])
            if m_l_freq > 0:
                m_log_l_freq = math.log10(m_l_freq)
                m_f_freq = float(celex2[j]['subtlex_f_freq'])
                m_f_prop = m_f_freq / m_l_freq
                if m_log_l_freq > (log_l_freq - 0.4) and m_log_l_freq < (log_l_freq + 0.4) and m_f_prop > (f_prop - 0.1) and m_f_prop < (f_prop + 0.1):
                    syl_matches[i].append(j)

ms = []
for i in syl_matches:
    ms.extend(syl_matches[i])

msu = list(set(ms))
