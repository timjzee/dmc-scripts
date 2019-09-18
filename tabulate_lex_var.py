import sys
import json
import codecs

# corpus = "ECSD"
tim_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

with codecs.open(tim_path + "Docs/lex_var_count.json", "r", "utf-8") as f:
    lex_var_count = json.load(f)

print("loading dictionaries")
canon_lex = {}
with codecs.open(tim_path + "clst-asr-fa/lexicon_from_MARIO_mod.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        pron = "".join(pron.split(" "))
        canon_lex[entry] = pron

with codecs.open(tim_path + "clst-asr-fa/oov_lex_comp-ECSD.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        pron = "".join(pron.split(" "))
        if entry not in canon_lex:
            canon_lex[entry] = pron

with codecs.open(tim_path + "clst-asr-fa/oov_lex_comp-IFADVcorpus.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        pron = "".join(pron.split(" "))
        if entry not in canon_lex:
            canon_lex[entry] = pron

print("loading expanded dictionaries")
exp_lex = {}
with codecs.open(tim_path + "clst-asr-fa/lexicon_ECSD.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        if "-nnn" in entry:
            entry = "".join(entry.split("-nnn"))
        pron = "".join(pron.split(" "))
        if entry in exp_lex:
            if pron not in exp_lex[entry]:
                exp_lex[entry].append(pron)
        else:
            exp_lex[entry] = [pron]

with codecs.open(tim_path + "clst-asr-fa/lexicon_IFADVcorpus.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        if "-nnn" in entry:
            entry = "".join(entry.split("-nnn"))
        pron = "".join(pron.split(" "))
        if entry in exp_lex:
            if pron not in exp_lex[entry]:
                exp_lex[entry].append(pron)
        else:
            exp_lex[entry] = [pron]


num_lines = len(lex_var_count)

header = "ort,num_poss_var,num_used_var,freq_canon,freq_other,freq_total\n"
with codecs.open(tim_path + "Docs/lex_var.csv", "w", "utf-8") as f:
    f.write(header)
    for line, ort in enumerate(lex_var_count, 1):
        print(line, "/", num_lines)
        num_used_var = len(lex_var_count[ort])
        if ort in exp_lex:
            num_poss_var = len(exp_lex[ort])
        else:
            num_poss_var = "NA"
        if ort not in canon_lex:
            freq_canon = "NA"
            freq_other = "NA"
            freq_total = 0
            for var in lex_var_count[ort]:
                freq_total += lex_var_count[ort][var]
        else:
            freq_other = 0
            freq_canon = 0
            for var in lex_var_count[ort]:
                if var == canon_lex[ort]:
                    freq_canon += lex_var_count[ort][var]
                else:
                    freq_other += lex_var_count[ort][var]
            freq_total = freq_canon + freq_other
        f.write("{},{},{},{},{},{}\n".format(ort, str(num_poss_var), num_used_var, str(freq_canon), str(freq_other), freq_total))
