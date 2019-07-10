import sys
import codecs
import re

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
corpus = "cgn"

print("Loading CELEX")
celex = {}
with codecs.open(tens_path + "other/DPW3.CD", "r", "utf-8") as f:
    for line in f:
        l_list = line[:-1].split("\\")
        word = l_list[1]
        syls = l_list[4].split("-")
        if syls == [""]:
            celex[word] = ["NA", "NA"]
        else:
            num_syl = len(syls)
            for counter, syl in enumerate(syls, 1):
                if "'" in syl:
                    stress = counter
                    break
            celex[word] = [str(num_syl), str(stress)]

print("Loading COW word frequencies")
cow = {}
with codecs.open(tens_path + "other/cow1.counts", "r", "utf-8") as f:
    for counter, line in enumerate(f, 1):
        wrd, freq = line[:-1].split("\t")
        cow[wrd] = freq
        if counter % 1000000 == 0:
            print(str((float(counter) / float(5052213)) * 100) + " %")

# print("Loading COW bigram frequencies")
# cow_big = {}
# with codecs.open(tens_path + "other/cow2.counts", "r", "utf-8") as f:
#    for counter, line in enumerate(f, 1):
#        bigram, freq = line[:-1].split("\t")
#        cow_big[bigram] = freq
#        if counter % 1000000 == 0:
#            print(str((float(counter) / float(73883219)) * 100) + " %")


with codecs.open(tens_path + corpus + "/eval_s_static_final2.csv", "w", "utf-8") as g:
    with codecs.open(tens_path + corpus + "/eval_s_static.csv", "r", "utf-8") as f:
        for i, line in enumerate(f, 1):
            print(i)
            l_list = line[:-1].split(",")
            if i == 1:
                inds = {col: index for index, col in enumerate(l_list, 0)}
                g.write(",".join(l_list + ["cow_wf"]) + "\n")
            else:
                # change stress info
                word = l_list[inds["word_ort"]]
                type_of_s = l_list[inds["type_of_s"]]
                num_syl, word_stress = celex[word] if word in celex else ["NA", "NA"]
                # if no syllable info in CELEX or affixed word not in CELEX
                if type_of_s not in ["S", "OTHER", "NA"] and num_syl == "NA" and word_stress == "NA":
                    word_stem = re.sub(r"'?s?$", "", word)
                    num_syl, word_stress = celex[word_stem] if word_stem in celex else ["NA", "NA"]
                l_list[inds["num_syl"]] = num_syl
                l_list[inds["word_stress"]] = word_stress
                # change wf to COW
                cow_word = re.sub(r"'", "", word.lower())
                cow_wf = cow[cow_word] if cow_word in cow else "0"
                g.write(",".join(l_list + [cow_wf]) + "\n")
