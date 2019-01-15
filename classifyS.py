import re


with open("/vol/tensusers/timzee/cgn/s_words_half.csv", "r") as f:
    f_text = f.readlines()

#f_text = re.sub(r'\n,', ',', f_text)
#f_lines = f_text.split("\n")

with open("/vol/tensusers/timzee/cgn/s_words_half_class.csv", "w") as f:
    f.write("component,language,filename,chunk_id,word_id,channel,chunk_start,chunk_end,word_index,word_ort,word_phon,next_phon,word_pos,word_class,type_of_s\n")

counter = 0
for line in f_lines:
    if counter == 0:
        counter += 1
        continue
    if counter % 1000 == 0:
        print(counter / len(f_lines) * 100, " percent")
    line_list = line.split(",")
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
    with open("/vol/tensusers/timzee/cgn/s_words_half_class.csv", "a") as f:
        f.write(",".join(line_list) + "," + pos_tag + "," + word_class + "," + type_of_s + "\n")
    counter += 1


