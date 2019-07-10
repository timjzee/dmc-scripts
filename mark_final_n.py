import sys
import codecs
import re

ifa_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"
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

with codecs.open(ifa_path + "fa_n_test3.txt", "w", "utf-8") as h:
    with codecs.open(tz_path + "clst-asr-fa/lexicon_n_test2.txt", "w", "utf-8") as f:
        with codecs.open(ifa_path + "eval_files_sen.txt", "r", "utf-8") as g:
            previous_words = []
            n_words = []
            for line in g:
                line_l = line.split(",")
                ort = line_l[4]
                print(line[:-1])
                words = ort.split(" ")
                ort_l = []
                for word in words:
                    if word not in previous_words:
                        previous_words.append(word)
                        for trans in kaldi_lex[word]:
                            if len(word) > 3:
                                if word[-3] not in vowels and word[-2:] == "en" and re.search(r'[euioa]+', word[:-2]) and trans[-1] == "n":
                                    n_words.append(word)
                                    f.write("{}-nnn\t{}\n".format(word, trans))
                                else:
                                    f.write("{}\t{}\n".format(word, trans))
                            else:
                                f.write("{}\t{}\n".format(word, trans))
                    if word in n_words:
                        ort_l.append("( {} | {}-nnn )".format(word, word))
                    else:
                        ort_l.append(word)
                h.write(",".join(line_l[:4] + [" ".join(ort_l)] + line_l[5:]))
