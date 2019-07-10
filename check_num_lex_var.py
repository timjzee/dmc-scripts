import subprocess
import sys
import codecs

home_dir = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

with codecs.open(home_dir + "clst-asr-fa/lexicon_from_MARIO.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        if entry[-2:] in ["en", "ën"] and pron[-1] == "@":
            pron = pron + " n"
        output = subprocess.check_output([home_dir + "fa_files/test_lex_exp.sh", entry + "\t" + pron, "1"]).decode("utf-8")
        output_l = [i for i in output.split("\n") if i != ""]
        num_vars = len(output_l)
        if num_vars == 1:
            print(entry, "\t", pron, "\t", len(output_l), "\n",)
