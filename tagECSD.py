from frog import Frog, FrogOptions
import glob
import re

txt_dict = {}

ecsd_path = "/vol/tensusers/timzee/ECSD/Annotations/"
pair_paths = glob.glob(ecsd_path + "ort/*/")
for pp in pair_paths:
    print(pp)
    pair = re.search(r"(?<=/)PP.*(?=/)", pp).group()
    spkr1 = pair.split("_")[0][2:]
    spkr2 = pair.split("_")[1]
    file_paths = glob.glob(pp + "*.TextGrid")
    txt_dict[pair] = {}
    for fp in file_paths:
        part = re.search(r"(?<=_)part.*(?=\.TextGrid)", fp).group()
#        print(part)
        txt_dict[pair][part] = {spkr1: "", spkr2: ""}
        spkr = ""
        with open(fp, "r") as f:
            for line in f:
                if "name =" in line:
                    spkr = re.search(r'(?<=name = ").*(?="[ ]*$)', line).group()
                if "text =" in line and spkr in [spkr1, spkr2]:
#                    print(line)
                    line_txt = re.search(r'(?<=text = ").*(?="[ ]*$)', line).group().encode("utf-8")
                    # remove codes, so they are not interpreted by Frog
                    line_txt = re.sub(r'\\[vVoO]', '', line_txt.decode("utf-8"))  # *u
                    line_txt = re.sub(r'\\\*', '', line_txt.strip(" "))         # *a
                    line_txt = re.sub(r'\\-', ' ', line_txt)                    # spelling
                    line_txt = re.sub(r'"', '', line_txt)
                    line_txt = re.sub(r'[ ]+(?=[.,:;?!])', "", line_txt)
#                    line_txt = re.sub(r'[\.!]*[!]+[\.!]*', '!', line_txt)   # replace combos including at least 1 '!'
#                    line_txt = re.sub(r'[\.!?]*[?]+[\.!?]*', '?', line_txt)  # replace combos including at least 1 '?'
#                    line_txt = re.sub(r'\.+', '.', line_txt)   # replace clusters of '.' with a single '.'
                    line_txt = re.sub(r'[!.?]+', '!', line_txt)
                    line_txt = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', line_txt)
                    line_txt = re.sub(r" 's ", " ''s ", line_txt)
                    line_txt = re.sub(r"^'s ", "''s ", line_txt)
#                    if re.search(r'"".+""', line_txt):
#                        print(re.search(r'"".+""', line_txt).group())
                    txt_dict[pair][part][spkr] += line_txt + " "

frog = Frog(FrogOptions(mwu=False, ner=False))

for pair in txt_dict:
    for part in txt_dict[pair]:
        with open("{}pos/{}/{}_{}.pos".format(ecsd_path, pair, pair, part), "w", encoding="utf-8") as g:
            for spkr in txt_dict[pair][part]:
                print(pair, part, spkr)
                text = txt_dict[pair][part][spkr]
                word_list = frog.process(text)
                s_counter = 0
                w_counter = 0
                for word in word_list:
                    if word["index"] == "1":
                        s_counter += 1
                        w_counter = 0
                        g.write("< file id: {}_{} speaker id: {} sentence: {} >\n".format(pair, part, spkr, s_counter))
                    if "LET" in word["pos"] and word["text"] != "&":
                        continue
                    else:
                        g.write("\t".join([word["text"], word["pos"], word["lemma"], str(word["posprob"])]) + "\n")
