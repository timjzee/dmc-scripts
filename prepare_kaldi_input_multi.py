import multiprocessing
import subprocess
import codecs
import sys
import os

home_dir = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"
tens_dir = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"

alphabet = {"a": "a", "b": "b e", "c": "s e", "d": "d e", "e": "e", "f": "E f", "g": "G e", "h": "h a", "i": "i", "j": "j e", "k": "k a", "l": "E l", "m": "E m", "n": "E n", "o": "o", "p": "p e", "q": "k y", "r": "E r", "s": "E s", "t": "t e", "u": "y", "v": "v e", "w": "w e", "x": "I k s", "y": "EI", "z": "z E t"}

graphemes = {"a": "A", "b": "b", "c": "k", "d": "d", "e": "E", "f": "f", "g": "G", "h": "h", "i": "I", "j": "j", "k": "k", "l": "l", "m": "m", "n": "n", "o": "O", "p": "p", "q": "k w", "r": "r", "s": "s", "t": "t", "u": "U", "v": "v", "w": "w", "x": "k s", "y": "i", "z": "z"}

with codecs.open(home_dir + "clst-asr-fa/alphemes.txt", "w", "utf-8") as f:
    for k in alphabet:
        f.write(k + "\t" + alphabet[k] + "\n")
        f.write(k + "\t" + graphemes[k] + "\n")

with codecs.open(tens_dir + "cgn/cgn_index_a_final.txt", "r", "utf-8") as f:
    cgn_index = f.readlines()

num_cores = 14
num_index_lines = len(cgn_index)
core_dict = {}
for i in range(num_cores):
    core_dict[str(i + 1)] = {}
    core_dict[str(i + 1)]["start"] = int(num_index_lines / num_cores) * i + 1
    if i + 1 != num_cores:
        core_dict[str(i + 1)]["end"] = int(num_index_lines / num_cores) * (i + 1)
    else:
        core_dict[str(i + 1)]["end"] = num_index_lines


def prepKaldi(core_num, start, end):
    subprocess.call(["python3", home_dir + "GitHub/dmc-scripts/prepare_kaldi_input.py", str(core_num), str(start), str(end)])


jobs = []
for core in range(num_cores):
    core_n = str(core + 1)
    s_line = core_dict[core_n]["start"]
    e_line = core_dict[core_n]["end"]
    p = multiprocessing.Process(target=prepKaldi, args=[core_n, s_line, e_line])
    jobs.append(p)
    p.start()

for job in jobs:
    job.join()

print("Combining files ...")

f = codecs.open(tens_dir + "cgn/prepared_index.txt", "w", "utf-8")
g = codecs.open(home_dir + "clst-asr-fa/oov_lex.txt", "w", "utf-8")
h = codecs.open(tens_dir + "cgn/oov_conv_table.txt", "w", "utf-8")
for core in range(num_cores):
    core_n = str(core + 1)
    with codecs.open(tens_dir + "cgn/prepared_index{}.txt".format(core_n), "r", "utf-8") as i:
        for l in i:
            f.write(l)
#    os.remove(tens_dir + "cgn/prepared_index{}.txt".format(core_n))
    with codecs.open(home_dir + "clst-asr-fa/oov_lex{}.txt".format(core_n), "r", "utf-8") as j:
        for l in j:
            g.write(l)
    os.remove(home_dir + "clst-asr-fa/oov_lex{}.txt".format(core_n))
    with codecs.open(tens_dir + "cgn/oov_conv_table{}.txt".format(core_n), "r", "utf-8") as k:
        for l in k:
            h.write(l)
    os.remove(tens_dir + "cgn/oov_conv_table{}.txt".format(core_n))
f.close()
g.close()
h.close()

print("Expanding Lexicon ...")

subprocess.call([home_dir + "fa_files/run_lexical_expansion.sh"])
