import tempfile
import textgrid
import sys
import json
import glob
import re
import codecs


corpus = "IFADVcorpus"
if corpus == "IFADVcorpus":
    o_path = "/tensusers/timzee/IFADVcorpus/kaldi_annot/v2/"
elif corpus == "cgn":
    component = "a"
    o_path = "/tensusers/timzee/" + corpus + "/kaldi_annot/comp-" + component + "/v2/"
else:
    o_path = "/tensusers/timzee/ECSD/kaldi_annot/v2/"


tg_folder = "/Volumes" + o_path if sys.platform == "darwin" else "/vol" + o_path
tim_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/home/timzee/Docs/"

try:
    f = codecs.open(tim_path + "lex_var_count.json", "r", "utf-8")
    lex_var_count = json.load(f)
    f.close
except:
    lex_var_count = {}

f = codecs.open(tim_path + "lex_var_count.json", "w", "utf-8")

print("Already", len(lex_var_count), "words in dictionary")

tg_paths = glob.glob(tg_folder + "*/*.awd") if corpus == "ECSD" else glob.glob(tg_folder + "*.awd")

for tg_path in tg_paths:
    print(tg_path)
    tg = textgrid.TextGrid()
    tg.read(tg_path)
    num_tiers = len(tg.tiers)
    num_speakers = num_tiers // 4
    for speaker in range(num_speakers):
        ort_tier = tg.tiers[speaker * 4].intervals
        meta_tier = tg.tiers[speaker * 4 + 1].intervals
        phon_tier = tg.tiers[speaker * 4 + 2].intervals
        for i, ort_interval in enumerate(ort_tier, 0):
            ort_mark = ort_interval.mark
            meta_mark = meta_tier[i].mark
            phon_mark = phon_tier[i].mark
            if meta_mark != "unintelligible" and phon_mark != "[SPN]":
                clean_ort = re.sub(r'\*[^ ]*', '', re.sub(r'[.!?\n]+', '', ort_mark)).lower()
                if clean_ort in lex_var_count:
                    if phon_mark in lex_var_count[clean_ort]:
                        lex_var_count[clean_ort][phon_mark] += 1
                    else:
                        lex_var_count[clean_ort][phon_mark] = 1
                else:
                    lex_var_count[clean_ort] = {phon_mark: 1}

print("Now", len(lex_var_count), "words in dictionary")

f.write(json.dumps(lex_var_count, ensure_ascii=False))
f.close()
