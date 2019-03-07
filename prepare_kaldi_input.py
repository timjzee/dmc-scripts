import sys
import codecs

home_dir = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"
tens_dir = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"

print("Loading KALDI lexicon")
kaldi_lex = {}
with codecs.open(home_dir + "clst-asr-fa/lexicon_from_MARIO.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        if entry in kaldi_lex:
            print(entry)
        else:
            kaldi_lex[entry] = pron


with codecs.open(home_dir + "Docs/oov_test.txt", "r", "utf-8") as f:
    cgn_index = f.readlines
