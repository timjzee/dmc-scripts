import sys
import codecs

lex_dir = "/Volumes/timzee/clst-asr-fa/" if sys.platform == "darwin" else "/home/timzee/clst-asr-fa/"

with codecs.open(lex_dir + "lexicon_from_MARIO_mod.txt", "w", "utf-8") as g:
    with codecs.open(lex_dir + "lexicon_from_MARIO.txt", "r", "utf-8") as f:
        for line in f:
            entry, pron = line[:-1].split("\t")
            if entry[-2:] in ["en", "ën"] and pron[-1] == "@":
                g.write(entry + "\t" + pron + " n\n")
            else:
                g.write(entry + "\t" + pron + "\n")
