import panphon.distance
import codecs
import sys
import time
import re

tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

wfed_threshold = 2
wfed_norm_threshold = 0.5
lev_threshold = 1
lev_norm_threshold = 0.25


kaldi2ipa = {}
with codecs.open(tz_path + "Docs/KALDI-CGN-IPA-WORD.txt", "r", "utf-8") as f:
    for c, i in enumerate(f, 0):
        if c > 0:
            l = i.split(",")
            kaldi2ipa[l[0]] = l[2]

lexicon = {}
with codecs.open(tz_path + "clst-asr-fa/lexicon_comp-ac.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        ipa = "".join([kaldi2ipa[p] for p in pron.strip(" ").split(" ")])
        lexicon[entry] = re.sub(r"ː", "", ipa)

from_w = 0
to_w = len(lexicon)
to_w = 100

keys = list(lexicon.keys())
# keys = ["buur", "boom", "bak", "bakker", "kaas", "melk", "eieren", "boek", "hond", "kip", "aanslag", "verdwaald", "grens", "grijns", "glijden", "begeleiden"]
dst = panphon.distance.Distance()
neighbour_lexicon = {}
t1 = time.time()
with codecs.open(tz_path + "Docs/neighbour_lexicon.txt", "w", "utf-8") as f:
    f.write("word\tlev_neighbours\tlev_density\tlev_norm_neighbours\tlev_norm_density\n")
    for counter, source in enumerate(keys[from_w:to_w], 1):
        print(counter, source)
        neighbour_lexicon[source] = {"lev": [], "lev_norm": []}
        for target in keys:
            if target != source:
                if target not in neighbour_lexicon:
                    source_phon = lexicon[source]
                    target_phon = lexicon[target]
#                    wfed = dst.weighted_feature_edit_distance(source_phon, target_phon)
                    min_len = min(len(source_phon), len(target_phon))
#                    wfed_norm = wfed / min_len
                    lev = dst.fast_levenshtein_distance(source_phon, target_phon)
                    lev_norm = lev / min_len
#                    if wfed <= wfed_threshold:
#                        neighbour_lexicon[source]["wfed"].append(target)
#                    if wfed_norm <= wfed_norm_threshold:
#                        neighbour_lexicon[source]["wfed_norm"].append(target)
                    if lev <= lev_threshold:
                        neighbour_lexicon[source]["lev"].append(target)
                    if lev_norm <= lev_norm_threshold:
                        neighbour_lexicon[source]["lev_norm"].append(target)
                else:   # avoid double calculations when target is already in lexicon
                    if source in neighbour_lexicon[target]["lev"]:
                        neighbour_lexicon[source]["lev"].append(target)
                    if source in neighbour_lexicon[target]["lev_norm"]:
                        neighbour_lexicon[source]["lev_norm"].append(target)
#                    if source in neighbour_lexicon[target]["wfed"]:
#                        neighbour_lexicon[source]["wfed"].append(target)
#                    if source in neighbour_lexicon[target]["wfed_norm"]:
#                        neighbour_lexicon[source]["wfed_norm"].append(target)
        f.write("\t".join([source, ",".join(neighbour_lexicon[source]["lev"]), str(len(neighbour_lexicon[source]["lev"])), ",".join(neighbour_lexicon[source]["lev_norm"]), str(len(neighbour_lexicon[source]["lev_norm"]))]) + "\n")

t2 = time.time()

print(t2 - t1)
