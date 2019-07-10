import sys
import codecs
import re

tens_path = "/Volumes/tensusers/timzee/cgn/n_tests/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/n_tests/"
tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

print("Loading lexicon\n")
kaldi_lex = {}
with codecs.open(tz_path + "clst-asr-fa/lexicon.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        if entry in kaldi_lex:
            kaldi_lex[entry].append(pron)
        else:
            kaldi_lex[entry] = [pron]

vowels = ["e", "u", "i", "o", "a"]

with codecs.open(tens_path + "prep_a_core_n.txt", "w", "utf-8") as h:
    with codecs.open(tz_path + "clst-asr-fa/lexicon_n_test_a.txt", "w", "utf-8") as f:
        with codecs.open(tens_path + "prep_a_core.txt", "r", "utf-8") as g:
            previous_words = []
            n_words = []
            for line_n, line in enumerate(g, 1):
                line_l = line.split(",")
                ort = line_l[4]
                ort = re.sub(r"\( ha \| haha \| hahaha \| hahahaha \| hahahahaha \| spn \)", "spn", ort)
                if ort != "ort":
                    print(line_n, line[:-1])
                    words = ort.split(" ")
                    ort_l = []
                    for word in words:
                        if word not in previous_words:
                            previous_words.append(word)
                            for trans in kaldi_lex[word]:
                                if word[-1] == "n" and trans[-1] == "n":
                                    n_words.append(word)
                                    f.write("{}-nnn\t{}\n".format(word, trans))
                                else:
                                    f.write("{}\t{}\n".format(word, trans))
                        if word in n_words:
                            ort_l.append("( {} | {}-nnn )".format(word, word))
                        else:
                            ort_l.append(word)
                    h.write(",".join(line_l[:4] + [" ".join(ort_l)] + line_l[5:]))
