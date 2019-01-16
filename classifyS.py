import re
import os


cgn_files = "/vol/bigdata/corpora2/CGN2/data/annot/"

with open("/vol/tensusers/timzee/cgn/s_words.csv", "r") as f:
    f_lines = f.readlines()

#f_text = re.sub(r'\n,', ',', f_text)
#f_lines = f_text.split("\n")

with open("/vol/tensusers/timzee/cgn/s_words_class.csv", "w") as f:
    f.write("component,language,filename,chunk_id,word_id,channel,chunk_start,chunk_end,word_ort,word_phon,next_phon,oov_in_chunk,word_pos,word_class,type_of_s,in_core\n")

counter = 0
for line in f_lines:
    line = re.sub(r'\n', "", line)
    if counter == 0:
        counter += 1
        continue
    if counter % 1000 == 0:
        print(counter / len(f_lines) * 100, " percent")
    line_list = line.split(",")
    in_core = "1" if os.path.isfile(cgn_files + "text/fon/" + "/".join(line_list[:3]) + ".fon.gz") else "0"
    pos_tag = re.search(r'[A-Z]+\(.*\)', line).group(0)
#    print(pos_tag)
    word_class = re.search(r'[A-Z]+(?=\()', pos_tag).group(0)
    pos_attr = pos_tag[len(word_class) + 1:-1].split(";")
#    print(word_class, pos_attr)
    if word_class == "N":
        if "mv" in pos_attr:
            type_of_s = "PL"
        elif "gen" in pos_attr:
            type_of_s = "GEN"
        else:
            type_of_s = "S"
    elif word_class == "ADJ":
        if "met-s" in pos_attr:
            type_of_s = "DER"
        elif "dim" in pos_attr:
            type_of_s = "OTHER"
        else:
            type_of_s = "S"
    elif word_class == "WW":
        type_of_s = "S"
    elif word_class == "TW":
        if "prenom" in pos_attr:
            if "bijz" in pos_attr:
                type_of_s = "GEN"
            else:
                type_of_s = "S"
        if "nom" in pos_attr:
            type_of_s = "OTHER"
        else:
            type_of_s = "S"
    elif word_class == "VNW":
        if "gen" in pos_attr:
            type_of_s = "GEN"
        else:
            type_of_s = "S"
    elif word_class == "LID":
        type_of_s = "OTHER"
    elif word_class == "SPEC" or word_class == "LET":
        type_of_s = "NA"
    else:
        type_of_s = "S"
    with open("/vol/tensusers/timzee/cgn/s_words_class.csv", "a") as f:
        f.write(",".join(line_list) + "," + word_class + "," + type_of_s + "," + in_core + "\n")
    counter += 1


