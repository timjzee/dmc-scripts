import json
import sys
import math

tens_path = "/Volumes/tensusers/timzee/other/" if sys.platform == "darwin" else "/vol/tensusers/timzee/other/"

with open(tens_path + "verbal_stems.json", "r") as f:
    celex2_vs = json.load(f)

with open(tens_path + "suitable_verbs.json", "r") as f:
    syl_matches = json.load(f)

# verbal stems and plural verbs already matched on lemma frequency

# let's do phrase-final first:

# most straight forward criterium is probably just the logged bigram frequency of [pronoun] + [verb]

syl_matches_sub = {}

for vs in syl_matches:
    syl_matches_sub[vs] = {}
    vs_log_bigram = math.log10(celex2_vs[vs]["chn_after_ik"]) if celex2_vs[vs]["chn_after_ik"] > 0 else -0.1
    for v in syl_matches[vs]:
        v_log_bigram = math.log10(syl_matches[vs][v]["chn_after_we"]) if syl_matches[vs][v]["chn_after_we"] > 0 else -0.1
        if vs_log_bigram > (v_log_bigram - 0.4) and vs_log_bigram < (v_log_bigram + 0.4):
            syl_matches_sub[vs][v] = syl_matches[vs][v]

for i in syl_matches_sub:
    print(i, len(syl_matches_sub[i]))

# some words cannot directly follow 'ik'
unacceptable_verbs = ["toeeigen", "wapen", "uitoefen", "uittoren", "vastketen", "meereken", "afreken", "afsteven", "aanreken", "aanteken", "afbaken"]

for i in syl_matches_sub:
    if i not in unacceptable_verbs:
        print(i, len(syl_matches_sub[i]))

print("beoefen", celex2_vs["beoefen"]["chn_after_ik"], celex2_vs["beoefen"]["verb_UP"], celex2_vs["beoefen"]["chn_before_end"])
for i in syl_matches_sub["beoefen"]:
    print(i, syl_matches_sub["beoefen"][i]["chn_after_we"], syl_matches_sub["beoefen"][i]["verb_UP"], syl_matches_sub["beoefen"][i]["chn_before_end"])


target_vs = "verreken"
print(target_vs, celex2_vs[target_vs]["chn_after_ik"], celex2_vs[target_vs]["verb_UP"], celex2_vs[target_vs]["chn_before_end"], celex2_vs[target_vs]["subtlex_l_freq"], celex2_vs[target_vs]["chn_total"])
for i in syl_matches_sub[target_vs]:
    print(i, syl_matches_sub[target_vs][i]["chn_after_we"], syl_matches_sub[target_vs][i]["verb_UP"], syl_matches_sub[target_vs][i]["chn_before_end"], int(syl_matches_sub[target_vs][i]["subtlex_l_freq"]), syl_matches_sub[target_vs][i]["chn_total"])
