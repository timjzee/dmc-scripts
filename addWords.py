import os
import re


f_path = "/Volumes/tensusers/timzee/cgn/" if os.name == "Darwin" else "/vol/tensusers/timzee/cgn/"

with open(f_path + "chunks.csv", "r") as f:
    f_lines = f.readlines()

f_lines2 = []
counter = 0
for fl in f_lines:
    counter += 1
    if counter == 1:
        continue
    new_line_list = re.sub(r'"', "", fl)[5:].split(",")
    f_lines2.append(["/".join(new_line_list[:3])] + new_line_list[3:])

with open(f_path + "chunks_KALDI.txt") as g:
    g_lines = g.readlines()

counter = 0
for l in g_lines:
    counter += 1
    if counter == 1:
        line_list = f_lines[0].split(",")
        with open(f_path + "chunks_PRAAT.txt", "w") as f:
            f.write("filepath," + re.sub(r'"', "", ",".join(line_list[3:-1])) + ",chunk_ort," + re.sub(r'"', "", line_list[-1]))
    l_list = l.split(",")
    for i in f_lines2:
        if i[:3] == l_list[:3]:
            with open(f_path + "chunks_PRAAT.txt", "a") as h:
                h.write(",".join(l_list[:-1]) + "," + i[-1])
