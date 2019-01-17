import re
import os


f_path = "/Volumes/tensusers/timzee/cgn/" if os.name == "Darwin" else "/vol/tensusers/timzee/cgn/"

with open(f_path + "chunks.csv", "r") as f:
    f_lines = f.readlines()

counter = 0
for line in f_lines:
    counter += 1
    line_list = line.split(",")
    if counter == 1:
        with open(f_path + "chunks2.txt", "w") as f:
            f.write("filepath," + ",".join(line_list[3:-1]) + "\n")
        continue
    new_line = re.sub(r'"', "", line)
    new_line = new_line[5:]
    new_line_list = new_line.split(",")
    with open(f_path + "chunks2.txt", "a") as f:
        f.write("/".join(new_line_list[:3]) + "," + ",".join(new_line_list[3:-1]) + "\n")
