import sys
import os
import timbl
import re
import json
import codecs
import multiprocessing

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"

merge = True
metric = "O"
nn_k = 4
dist_weight = "ID"

korte_vocalen = ["I", "E", "A", "O", "}"]
lange_vocalen = ["i", "y", "e", "|", "a", "o", "u"]
diftongen = ["K", "L", "M"]
schwa = ["@"]
leen_vocalen = [")", "*", "<", "!", "("]
klinkers = korte_vocalen + lange_vocalen + diftongen + schwa + leen_vocalen
occlusieven = ["p", "b", "t", "d", "k", "g"]
fricatieven = ["f", "v", "s", "z", "S", "Z", "x", "G", "h"]
nasalen = ["m", "n", "N"]
liquidae = ["l", "r"]
halfvocalen = ["j", "w"]
medeklinkers = occlusieven + fricatieven + nasalen + liquidae + halfvocalen


def getFeatures(word, dataset, num_s):
    syllables = dataset[word]['features'].split("-")
    extra_syls = len(syllables) - num_s
    syllables = syllables[-num_s:] if extra_syls >= 0 else abs(extra_syls) * ["="] + syllables
    features = []
    stress = []
    for syl in syllables:
        if syl == "=":
            features.extend(["=", "=", "="])
            stress.append("-")
        else:
            # check stress
            if syl[0] == "'":
                stress.append("+")
                syl = syl[1:]
            else:
                stress.append("-")
            # find onset, nucleus, and coda
            onset = re.search(r'^[{}]+'.format("".join(medeklinkers)), syl)
            onset = onset.group() if onset else "="
            nucleus = re.search(r'[{}]+'.format("".join(klinkers)), syl)
            nucleus = nucleus.group() if nucleus else "="
            coda = re.search(r"[{}]+$".format("".join(medeklinkers)), syl)
            coda = coda.group() if coda else "="
            features.extend([onset, nucleus, coda])
    return features, stress


with codecs.open(tens_path + "other/dataset_invar.json", "r") as f:     # maybe replace this with only the invar nouns that were used for var p(s)
    dataset_invar = json.load(f)


# for num, word in enumerate(dataset_invar, 1):
def run_timbl(word):
    feat, strs = getFeatures(word, dataset_invar, 2)
    classifier = timbl.TimblClassifier("invar_test_" + word, "-m{} -k {} -d {} -G 0".format(metric, nn_k, dist_weight), dist=True)
    merge_lines = []
    for word2 in dataset_invar:
        feat2, strs2 = getFeatures(word2, dataset_invar, 2)
        pl_class = dataset_invar[word2]['class']
        if word != word2:
            if merge:
                if feat2 + strs2 + [word2[-1]] + [pl_class] not in merge_lines:
                    classifier.append(tuple(feat2 + strs2 + [word2[-1]]), pl_class)
                    merge_lines.append(feat2 + strs2 + [word2[-1]] + [pl_class])
            else:
                classifier.append(tuple(feat2 + strs2 + [word2[-1]]), pl_class)
    classifier.train()
    classifier.api.getWeights("invar_test_best.wgt", classifier.api.currentWeighting())
    classlabel, distribution, distance = classifier.classify(tuple(feat + strs + [word[-1]]))
    if os.path.exists(tens_path + "other/invar_test_" + word + ".train"):
        os.remove(tens_path + "other/invar_test_" + word + ".train")
#    result_dict[word] = distribution
    for pl in ["EN", "S", "OTHER"]:
        if pl not in distribution:
            distribution[pl] = 0
    print("\n{},{},{},{}\n".format(word, distribution["EN"], distribution["S"], distribution["OTHER"]))
    return "{},{},{},{}\n".format(word, distribution["EN"], distribution["S"], distribution["OTHER"])


# test_dict = {"zwaard": {"features": "'zwart", "class": "EN", "freq": 206}, "zwerfvogel": {"features": "'zwEr-'fo-G@l", "class": "S", "freq": 1}}

# test_dict = {k: dataset_invar[k] for num, k in enumerate(dataset_invar, 1) if num < 50}

# test_dict = copy.deepcopy(dataset_invar)

p = multiprocessing.Pool(60)

result_list = p.map(run_timbl, dataset_invar)

# result_dict = {}
with codecs.open(tens_path + "other/invar_probs.csv", "w") as f:
    f.write("word,p_en,p_s,p_other\n")
    for line in result_list:
        f.write(line)
#        f.write("{},{},{},{}\n".format(w, result_dict[w]["EN"], result_dict[w]["S"], result_dict[w]["OTHER"]))
