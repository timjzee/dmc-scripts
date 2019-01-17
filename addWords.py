import os


f_path = "/Volumes/tensusers/timzee/cgn/" if os.name == "Darwin" else "/vol/tensusers/timzee/cgn/"

with open(f_path + "chunks.csv", "r") as f:
    f_lines = f.readlines()

with open(f_path + "chunks_KALDI.txt") as g:
    g_lines = g.readlines()

with open(f_path + "chunks_PRAAT.txt", "w") as h:
    h.write(f_lines[0])

counter = 0
for l in g_lines:
    with open(f_path + "chunks_PRAAT.txt", "a") as h:
        h.write(l[:-1] + "," + f_lines[counter + 1].split(",")[-1])
    counter += 1
