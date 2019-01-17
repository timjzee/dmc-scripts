import os


f_path = "/Volumes/tensusers/timzee/cgn/" if os.name == "Darwin" else "/vol/tensusers/timzee/cgn/"

with open(f_path + "chunks.csv", "r") as f:
    f_lines = f.readlines()

with open(f_path + "chunks_KALDI.txt") as g:
    g_lines = g.readlines()

with open(f_path + "chunks_PRAAT.txt", "w") as h:
    h.write(f_lines[0] + "\n")

counter = 0
for l in g_lines:
    counter += 1
    with open(f_path + "chunks_PRAAT.txt", "a") as h:
        h.write(l + f_lines[counter] + "\n")
