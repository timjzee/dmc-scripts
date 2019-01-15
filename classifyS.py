import re


with open("/vol/tensusers/timzee/cgn/s_words.csv", "r") as f:
    f_lines = f.readlines()

for line in f_lines:
    line_list = line.split(",")
    pos_index = len(line_list) - 1
    
