import re
import sys
import math
import multiprocessing as mp
import json

tens_path = "/Volumes/tensusers/timzee/other/" if sys.platform == "darwin" else "/vol/tensusers/timzee/other/"

print("Loading CHN1 data")
# keep differences in upper/lower case
chn_uni = {}
with open(tens_path + "CHN-ngrams1.0/chn1n_sorted.tab", "r") as f:
    for l in f:
        freq, form = l[:-1].split("\t")
        if len(form) > 0:
            chn_uni[form] = int(freq)

keys = list(chn_uni.keys())

num_cores = 20
num_lex_lines = len(keys)
# num_lex_lines = 1000
core_dict = {}
for i in range(num_cores):
    core_dict[str(i + 1)] = {}
    core_dict[str(i + 1)]["start"] = int(num_lex_lines / num_cores) * i + 1
    if i + 1 != num_cores:
        core_dict[str(i + 1)]["end"] = int(num_lex_lines / num_cores) * (i + 1)
    else:
        core_dict[str(i + 1)]["end"] = num_lex_lines

verbal_stem_infs = {}
with open(tens_path + "suitable_verbal_stems2.txt", "r") as f:
    for l in f:
        verbal_stem_infs[l[:-1].split(",")[0]] = {"subtlex_l_freq": l[:-1].split(",")[-1], "subtlex_f_freq": l[:-1].split(",")[2]}

print("Loading CELEX data")
celex_vs = {}
celex = {}
with open(tens_path + "DML.CD", "r") as f:
    for l in f:
        l_id, wrd, lem_freq = l.split("\\")[:3]
        if wrd == "rontgenen":
            wrd = "röntgenen"
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
            celex2[wrd] = {"subtlex_l_freq": 0, "subtlex_f_freq": 0, "celex_l_freq": celex[l_id]["celex_l_freq"], "celex_f_freq": f_freq, "n_syl": num_syl, "stress": stress, "phon": phon, "syl_struc": syl_struc}
        elif f_id in f_id_list_vs and l_id in celex_vs:
            if wrd == "rontgen":
                wrd = "röntgen"
            syls = [i for i in phon.split("-") if i != ""]
            num_syl = len(syls)
            counter = 0
            for syl in syls:
                counter += 1
                if "'" in syl:
                    break
            stress = counter
            celex2_vs[wrd] = {"subtlex_l_freq": verbal_stem_infs[wrd + "en"]["subtlex_l_freq"], "subtlex_f_freq": verbal_stem_infs[wrd + "en"]["subtlex_f_freq"], "celex_l_freq": celex_vs[l_id]["celex_l_freq"], "celex_f_freq": f_freq, "n_syl": num_syl, "stress": stress, "phon": phon, "syl_struc": syl_struc}

print("Loading SUBTLEX data")
with open(tens_path + "SUBTLEX-NL.master.txt", "r") as f:
    for l in f:
        l_list = l[:-1].split("\t")
        if l_list[0] in celex2 and l_list[1] == "WW":
            celex2[l_list[0]]["subtlex_l_freq"] += int(l_list[4])
        elif l_list[2] in celex2 and l_list[3].split(",")[0] in ["inf", "pv"]:
            celex2[l_list[2]]["subtlex_f_freq"] += int(l_list[4])

# get measures for conditional probabilities
#   probability of verb given previous context
#   probability of following context given verb
#
# first get frequency of sentence starts (and thereby automatically number of sentence ends)
#   find all words that occur more frequently with first letter lowercase than uppercase
#   chn_lower_upper = sum of the frequency of all occurences of lowercase > uppercase words beginning with uppercase

#   now we still need to account for words that start more frequently with uppercase (e.g. names) used at the start of sentence
#   we approximate this number as follows:
#       chn_lower_lower = sum of the frequency of all occurences of lowercase > uppercase words beginning with lowercase
#       calculate average proportion of sentence starts: chn_upper_lower_prop = chn_lower_upper / (chn_lower_upper + chn_lower_lower)
#       chn_upper_upper = sum of the frequency of all occurences of lowercase <= uppercase words beginning with uppercase
#       chn_upper_upper_extra = chn_upper_lower_prop * chn_upper_upper
#   chn_total_sentences = chn_lower_upper + chn_upper_upper_extra


def getNumSentences(from_w, to_w, core):
    prev_words = []
#    upper_dom = []
    chn_upper_upper = 0
#    lower_dom = []
    chn_lower_upper = 0
    chn_lower_lower = 0
    counter = 0
    for w in keys[from_w:to_w + 1]:
        counter += 1
        if counter % 1000 == 0:
            print(core, counter)
        if w.lower() in prev_words:
            continue
        prev_words.append(w.lower())
        if w[0].isupper():
            upper_f = chn_uni[w]
            lower_w = w.lower()
            lower_f = chn_uni[lower_w] if lower_w in chn_uni else 0
        elif w.islower():
            lower_f = chn_uni[w]
            upper_w = w[0].upper() + w[1:]
            upper_f = chn_uni[upper_w] if upper_w in chn_uni else 0
            upper_w2 = w.upper()
            upper_f += chn_uni[upper_w2] if upper_w2 in chn_uni else 0
        else:
            continue
        if lower_f > upper_f:
#            lower_dom.append(w.lower())
            chn_lower_upper += upper_f
            chn_lower_lower += lower_f
        else:
#            upper_dom.append(w.lower())
            chn_upper_upper += upper_f
    q.put([chn_upper_upper, chn_lower_upper, chn_lower_lower])


print("Multiprocessing")

q = mp.Queue()

jobs = []
for core in range(num_cores):
    core_n = str(core + 1)
    s_word = core_dict[core_n]["start"]
    e_word = core_dict[core_n]["end"]
    p = mp.Process(target=getNumSentences, args=[s_word, e_word, core_n])
    jobs.append(p)
    p.start()

results = []
while len(results) < num_cores:
    results.append(q.get())

for job in jobs:
    job.join()

chn_upper_upper = 0
chn_lower_upper = 0
chn_lower_lower = 0
for r in results:
    chn_upper_upper += r[0]
    chn_lower_upper += r[1]
    chn_lower_lower += r[2]

chn_upper_lower_prop = chn_lower_upper / (chn_lower_upper + chn_lower_lower)
chn_upper_upper_extra = chn_upper_lower_prop * chn_upper_upper
chn_total_sentences = chn_lower_upper + chn_upper_upper_extra

print("Loading CHN2 data")
chn_bi = {}
with open(tens_path + "CHN-ngrams1.0/chn2n_sorted.tab", "r") as f:
    for l in f:
        freq, form = l[:-1].split("\t")
        if len(form) > 0:
            chn_bi[form] = int(freq)

keys_bi = list(chn_bi.keys())
a_A_index = keys_bi.index("a A")


def getBiFreqs(verb, next_w, prev_w):
    verb_upper = verb[0].upper() + verb[1:]
    chn_upper = chn_uni[verb_upper] if verb_upper in chn_uni else 0
    chn_total = chn_upper + chn_uni[verb] if verb in chn_uni else chn_upper
    chn_before_pron = chn_bi[verb + " " + next_w] if verb + " " + next_w in chn_bi else 0
    chn_before_pron += chn_bi[verb_upper + " " + next_w] if verb_upper + " " + next_w in chn_bi else 0
    chn_after_pron = chn_bi[prev_w + " " + verb] if prev_w + " " + verb in chn_bi else 0
    chn_after_pron += chn_bi[prev_w[0].upper() + prev_w[1:] + " " + verb] if prev_w[0].upper() + prev_w[1:] + " " + verb in chn_bi else 0
    chn_before_end = 0
    for bi in keys_bi[a_A_index:]:
        word1, word2 = bi.split(" ")
        if word1 == verb and len(word2) > 0:
            if re.search(r'[A-Z]', word2[0]):
                chn_before_end += chn_bi[bi]
            else:
                break
    return chn_upper, chn_total, chn_before_pron, chn_after_pron, chn_before_end


verb_keys = list(celex2.keys())


def getVUP(verb, phon):     # we're taking lexical stress into account, so UP of be're.ken is 4 because be,re.de'ne.ren has different main stress on [e]
    phon_l = [s.strip("'") for s in phon.split("-")]
    n_phon = len("".join(phon_l))
    if n_phon == 0:
        return None
    found_letter = False
    wrong_letter = False
    max_vup = 0
    v_i = -1
    while not (found_letter and wrong_letter) and v_i + 1 < len(verb_keys):
        v_i += 1
        v = verb_keys[v_i]
        if verb == v:
            continue
        wrong_letter = verb[0] != v[0]
        if not wrong_letter:
            if not found_letter:
                found_letter = True
            vup = 0
            v_phon = celex2[v]["phon"]
            p_mismatch = False
            for p_i, p in enumerate(phon, 0):
                if p_i >= len(v_phon):
                    break
                if p != v_phon[p_i]:
                    p_mismatch = True
                    break
                else:
                    if p not in ["'", "-"]:
                        vup += 1
            if vup >= n_phon:  # we found a verb that contains all phones of the target verb, i.e., target verb does not have a UP
                max_vup = 0
                break
            if p_mismatch and vup + 1 > max_vup and int(celex2[v]["celex_l_freq"]) > 0:    # we only count words with celex_freq > 0
                max_vup = vup + 1
        if v_i + 1 == len(verb_keys):
            wrong_letter = True
    return max_vup


print("Get probability data for verbal stems and plural verbs")

chn_total_jij = chn_uni["jij"]
chn_total_jij += chn_uni["Jij"]
chn_total_ik = chn_uni["ik"]
chn_total_ik += chn_uni["Ik"]

chn_total_jullie = chn_uni["jullie"]
chn_total_jullie += chn_uni["Jullie"]
chn_total_we = chn_uni["we"]
chn_total_we += chn_uni["We"]

# note that celex_f_freq in celex2_vs represents different inflections than celex_f_freq in celex2
# find matches in terms of n_syl, same length of vowels, stress, frequencies


def getProbs(from_v, to_v, c_num):
    syl_matches = {}
    vs_part = {}
    for i in vs_keys[from_v:to_v]:
        vs_part[i] = celex2_vs[i]
        print("Core " + str(c_num) + " getting bigram probs for " + i)
        vs_part[i]["chn_upper"], vs_part[i]["chn_total"], vs_part[i]["chn_before_jij"], vs_part[i]["chn_after_ik"], vs_part[i]["chn_before_end"] = getBiFreqs(i, "jij", "ik")
        vs_part[i]["pr_verb_given_start"] = vs_part[i]["chn_upper"] / chn_total_sentences
        vs_part[i]["pr_verb_given_jij"] = vs_part[i]["chn_before_jij"] / chn_total_jij
        vs_part[i]["pr_jij_given_verb"] = vs_part[i]["chn_before_jij"] / vs_part[i]["chn_total"] if vs_part[i]["chn_total"] > 0 else None
        vs_part[i]["pr_verb_given_ik"] = vs_part[i]["chn_after_ik"] / chn_total_ik
        vs_part[i]["pr_verb_given_end"] = vs_part[i]["chn_before_end"] / chn_total_sentences
        vs_part[i]["pr_end_given_verb"] = vs_part[i]["chn_before_end"] / vs_part[i]["chn_total"] if vs_part[i]["chn_total"] > 0 else None
        vs_part[i]["verb_UP"] = getVUP(i, vs_part[i]["phon"])
        l_freq = float(vs_part[i]['subtlex_l_freq'])
        log_l_freq = math.log10(l_freq)
    #    f_freq = float(vs_part[i]['subtlex_f_freq'])
    #    f_prop = f_freq / l_freq
        syl_matches[i] = {}
        for j in celex2:
            if vs_part[i]["stress"] == celex2[j]["stress"] and "".join(vs_part[i]["syl_struc"].split("C")) == "".join(celex2[j]["syl_struc"].split("C")):
                m_l_freq = float(celex2[j]['subtlex_l_freq'])
                if m_l_freq > 0:
                    m_log_l_freq = math.log10(m_l_freq)
                    m_f_freq = float(celex2[j]['subtlex_f_freq'])
    #                m_f_prop = m_f_freq / m_l_freq
                    if m_log_l_freq > (log_l_freq - 0.4) and m_log_l_freq < (log_l_freq + 0.4):  # and m_f_prop > (f_prop - 0.1) and m_f_prop < (f_prop + 0.1):
                        syl_matches[i][j] = {'subtlex_l_freq': m_l_freq, 'subtlex_f_freq': m_f_freq}
                        syl_matches[i][j]["chn_upper"], syl_matches[i][j]["chn_total"], syl_matches[i][j]["chn_before_jullie"], syl_matches[i][j]["chn_after_we"], syl_matches[i][j]["chn_before_end"] = getBiFreqs(j, "jullie", "we")
                        syl_matches[i][j]["pr_verb_given_start"] = syl_matches[i][j]["chn_upper"] / chn_total_sentences
                        syl_matches[i][j]["pr_verb_given_jullie"] = syl_matches[i][j]["chn_before_jullie"] / chn_total_jullie
                        syl_matches[i][j]["pr_jullie_given_verb"] = syl_matches[i][j]["chn_before_jullie"] / syl_matches[i][j]["chn_total"] if syl_matches[i][j]["chn_total"] > 0 else None
                        syl_matches[i][j]["pr_verb_given_we"] = syl_matches[i][j]["chn_after_we"] / chn_total_we
                        syl_matches[i][j]["pr_verb_given_end"] = syl_matches[i][j]["chn_before_end"] / chn_total_sentences
                        syl_matches[i][j]["pr_end_given_verb"] = syl_matches[i][j]["chn_before_end"] / syl_matches[i][j]["chn_total"] if syl_matches[i][j]["chn_total"] > 0 else None
                        syl_matches[i][j]["verb_UP"] = getVUP(j, celex2[j]["phon"])
    q.put([vs_part, syl_matches])


num_cores = 14
vs_keys = list(celex2_vs.keys())
max_l_per_c = math.ceil(len(vs_keys) / num_cores)
n_max_c = int(len(vs_keys) - (max_l_per_c - 1) * num_cores)

core_dict2 = {}
for i in range(num_cores):
    core_dict2[str(i + 1)] = {}
    if i < n_max_c:
        core_dict2[str(i + 1)]["start"] = max_l_per_c * i
        core_dict2[str(i + 1)]["end"] = max_l_per_c * (i + 1)
    else:
        core_dict2[str(i + 1)]["start"] = max_l_per_c * n_max_c + (max_l_per_c - 1) * (i - n_max_c)
        if i + 1 != num_cores:
            core_dict2[str(i + 1)]["end"] = max_l_per_c * n_max_c + (max_l_per_c - 1) * (i - n_max_c + 1)
        else:
            core_dict2[str(i + 1)]["end"] = len(vs_keys)


print("Multiprocessing")

q = mp.Queue()

jobs = []
for core in range(num_cores):
    core_n = str(core + 1)
    s_word = core_dict2[core_n]["start"]
    e_word = core_dict2[core_n]["end"]
    p = mp.Process(target=getProbs, args=[s_word, e_word, core_n])
    jobs.append(p)
    p.start()

results = []
while len(results) < num_cores:
    results.append(q.get())

for job in jobs:
    job.join()

celex2_vs2 = {}
syl_matches2 = {}
for i in results:
    for j in i[0]:
        celex2_vs2[j] = i[0][j]
    for k in i[1]:
        syl_matches2[k] = i[1][k]

print("Writing data")

with open(tens_path + "verbal_stems.json", "w") as f:
    json.dump(celex2_vs2, f)

with open(tens_path + "suitable_verbs.json", "w") as f:
    json.dump(syl_matches2, f)

# ms = []
# for i in syl_matches:
#     ms.extend(list(syl_matches[i].keys()))

# msu = list(set(ms))
