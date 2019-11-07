import multiprocessing
import panphon.distance
import codecs
import sys
import re
import time
import os

tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"
tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"


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

# replace with COW frequencies
print("Loading SUBTLEX")
subtlex = {}
with codecs.open(tens_path + "other/SUBTLEX-NL.txt", "r", "utf-8") as f:
    for line in f:
        line_list = line[:-1].split("\t")
        word = line_list[0]
        subtlexwf = line_list[6]
        freqcount = line_list[1]
        subtlex[word] = [subtlexwf, freqcount]

print("Loading lexicon")
lexicon = {}
with codecs.open(tz_path + "clst-asr-fa/lexicon_from_MARIO.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        ipa = "".join([kaldi2ipa[p] for p in pron.strip(" ").split(" ")])
        lexicon[entry] = re.sub(r"ː", "", ipa)

num_cores = 40
num_lex_lines = len(lexicon)
# num_lex_lines = 1000
core_dict = {}
for i in range(num_cores):
    core_dict[str(i + 1)] = {}
    core_dict[str(i + 1)]["start"] = int(num_lex_lines / num_cores) * i + 1
    if i + 1 != num_cores:
        core_dict[str(i + 1)]["end"] = int(num_lex_lines / num_cores) * (i + 1)
    else:
        core_dict[str(i + 1)]["end"] = num_lex_lines

# from_w = 0
# to_w = len(lexicon)
# to_w = 100

keys = list(lexicon.keys())
# keys = ["buur", "boom", "bak", "bakker", "kaas", "melk", "eieren", "boek", "hond", "kip", "aanslag", "verdwaald", "grens", "grijns", "glijden", "begeleiden"]
dst = panphon.distance.Distance()


def getNeighbours(from_w, to_w, core):
    neighbour_lexicon = {}
    for counter, source in enumerate(keys[from_w:to_w + 1], 1):
        print(core, counter, source)
        neighbour_lexicon[source] = {"lev": [], "lev_freq": [], "lev_norm": [], "lev_norm_freq": []}
        for target in keys:
            if target != source:
                if target not in neighbour_lexicon:
                    source_phon = lexicon[source]
                    target_phon = lexicon[target]
                    min_len = min(len(source_phon), len(target_phon))
                    lev = dst.fast_levenshtein_distance(source_phon, target_phon)
                    lev_norm = lev / min_len
                    if lev <= lev_threshold:
                        neighbour_lexicon[source]["lev"].append(target)
                        if target in subtlex:
                            neighbour_lexicon[source]["lev_freq"].append(int(subtlex[target][1]))
                    if lev_norm <= lev_norm_threshold:
                        neighbour_lexicon[source]["lev_norm"].append(target)
                        if target in subtlex:
                            neighbour_lexicon[source]["lev_norm_freq"].append(int(subtlex[target][1]))
                else:   # avoid double calculations when target is already in lexicon
                    if source in neighbour_lexicon[target]["lev"]:
                        neighbour_lexicon[source]["lev"].append(target)
                        if target in subtlex:
                            neighbour_lexicon[source]["lev_freq"].append(int(subtlex[target][1]))
                    if source in neighbour_lexicon[target]["lev_norm"]:
                        neighbour_lexicon[source]["lev_norm"].append(target)
                        if target in subtlex:
                            neighbour_lexicon[source]["lev_norm_freq"].append(int(subtlex[target][1]))
    with codecs.open(tz_path + "Docs/neighbours" + core + ".txt", "w", "utf-8") as f:
        for s in neighbour_lexicon:
            lev_freq = str(sum(neighbour_lexicon[s]["lev_freq"])) if len(neighbour_lexicon[s]["lev_freq"]) > 0 else "0"
            lev_norm_freq = str(sum(neighbour_lexicon[s]["lev_norm_freq"])) if len(neighbour_lexicon[s]["lev_norm_freq"]) > 0 else "0"
            f.write("\t".join([s, ",".join(neighbour_lexicon[s]["lev"]), str(len(neighbour_lexicon[s]["lev"])), lev_freq, ",".join(neighbour_lexicon[s]["lev_norm"]), str(len(neighbour_lexicon[s]["lev_norm"])), lev_norm_freq]) + "\n")
    print("CORE ", core, " IS DONE!!!!!!!!")


#lexicon_queue = multiprocessing.Queue()

t1 = time.time()

jobs = []
for core in range(num_cores):
    core_n = str(core + 1)
    s_word = core_dict[core_n]["start"]
    e_word = core_dict[core_n]["end"]
    p = multiprocessing.Process(target=getNeighbours, args=[s_word, e_word, core_n])
    jobs.append(p)
    p.start()

print("AAAAAAAAAAAAAAAAAAAAAAAAA")

for job in jobs:
    job.join()

print("BBBBBBBBBBBBBBBBBBBBBBBBB")

#nl = {}
#while len(nl) < num_lex_lines:
#    wrd, nghbrs = lexicon_queue.get()
#    nl[wrd] = nghbrs

with codecs.open(tz_path + "Docs/neighbour_lexicon.txt", "w", "utf-8") as f:
    for c in core_dict:
        with codecs.open(tz_path + "Docs/neighbours" + c + ".txt", "r", "utf-8") as g:
            for line in g:
                f.write(line)
        os.remove(tz_path + "Docs/neighbours" + c + ".txt")

print(time.time() - t1)
