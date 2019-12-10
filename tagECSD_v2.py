from frog import Frog, FrogOptions
import glob
import re
import multiprocessing

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
                    line_txt = re.sub(r'[\s]+', ' ', line_txt)
                    line_txt = line_txt.strip(" ")
#                    line_txt = re.sub(r'\\\*$', '!', line_txt)  # prevent from being interpreted as SPEC(afk)
                    line_txt = re.sub(r'\\\*', '', line_txt)         # *a
                    line_txt = re.sub(r'\\-', ' ', line_txt)                    # spelling
                    line_txt = re.sub(r'"', '', line_txt)
                    line_txt = re.sub(r'[ ]+(?=[.,:;?!])', "", line_txt)
                    line_txt = re.sub(r'[\.!]*[!]+[\.!]*', '!', line_txt)   # replace combos including at least 1 '!'
                    line_txt = re.sub(r'[\.!?]*[?]+[\.!?]*', '?', line_txt)  # replace combos including at least 1 '?'
                    line_txt = re.sub(r'\.+', '.', line_txt)   # replace clusters of '.' with a single '.'
                    line_txt = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', line_txt)
                    line_txt = re.sub(r" 's ", " ''s ", line_txt)  # to make sure that preceding punctuation results in sentence demarcation
                    line_txt = re.sub(r"^'s ", "''s ", line_txt)
                    line_txt = re.sub(r'[!?\.,:;]', lambda m: " " + m.group(), line_txt)  # prevent from being interpreted as SPEC(afk)
#                    if re.search(r'"".+""', line_txt):
#                        print(re.search(r'"".+""', line_txt).group())
                    if len(line_txt) > 0:
                        if line_txt[-1] not in [".", ",", "!", "?", ":", ";"]:  # add . if chunk does not end in punctuation
#                            if re.search(r' [A-Za-z]$', line_txt):  # prevent from being interpreted as SPEC(afk)
#                                line_txt += "!"
#                            else:
                            line_txt += " ."
                    txt_dict[pair][part][spkr] += line_txt + " "

frog = Frog(FrogOptions(parser=True))

num_cores = 5
num_index_lines = len(txt_dict)
txt_dict_keys = list(txt_dict.keys())
core_dict = {}
for i in range(num_cores):
    core_dict[str(i + 1)] = {}
    core_dict[str(i + 1)]["start"] = int(num_index_lines / num_cores) * i + 1
    if i + 1 != num_cores:
        core_dict[str(i + 1)]["end"] = int(num_index_lines / num_cores) * (i + 1)
    else:
        core_dict[str(i + 1)]["end"] = num_index_lines
    core_dict[str(i + 1)]["pairs"] = txt_dict_keys[core_dict[str(i + 1)]["start"] - 1:core_dict[str(i + 1)]["end"]]


def tag_pairs(pairs):
    for pair in pairs:
        for part in txt_dict[pair]:
            with open("{}pos2/{}/{}_{}.pos".format(ecsd_path, pair, pair, part), "w", encoding="utf-8") as g:
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
    #                    if "LET" in word["pos"] and word["text"] != "&":
    #                        continue
    #                    else:

                        if "_" in word["text"]:
                            for i_num, i in enumerate(word["text"].split("_"), 0):
                                g.write("\t".join([i, word["pos"].split("_")[i_num], word["lemma"].split("_")[i_num], str(word["posprob"]), word["dep"], str(word["depindex"]), str(word["index"]), "1"]) + "\n")
                        else:
                            g.write("\t".join([word["text"], word["pos"], word["lemma"], str(word["posprob"]), word["dep"], str(word["depindex"]), str(word["index"]), "0"]) + "\n")


jobs = []
for core in range(num_cores):
    core_n = str(core + 1)
    prs = core_dict[core_n]["pairs"]
    p = multiprocessing.Process(target=tag_pairs, args=[prs])
    jobs.append(p)
    p.start()

for job in jobs:
    job.join()
