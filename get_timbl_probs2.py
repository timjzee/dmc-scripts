import sys
import codecs
import re

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"

print("Loading Timbl probabilities")
timbl = {}
with codecs.open(tens_path + "other/pl_type_timbl_k2_ID_verbs-VERB.csv", "r", "utf-8") as f:
    for line in f:
        word = line[:-1].split(",")[0]
        prob_strings = re.search(r'(?<={ ).*(?= }$)', line[:-1]).group().split(", ")
        timbl[word] = {}
        for plural_prob in prob_strings:
            plural, prob = plural_prob.split(" ")
            timbl[word][plural] = float(prob)

with codecs.open(tens_path + "other/pl_type_probs_k2_ID_verbs-VERB.csv", "w", "utf-8") as f:
    for word in timbl:
        s = timbl[word]["S"] if "S" in timbl[word] else 0
        en = timbl[word]["EN"] if "EN" in timbl[word] else 0
        other = timbl[word]["OTHER"] if "OTHER" in timbl[word] else 0
        s2 = str(s / (s + en + other))
        en2 = str(en / (s + en + other))
        other2 = str(other / (s + en + other))
        f.write(word + "," + s2 + "," + en2 + "," + other2 + "\n")
