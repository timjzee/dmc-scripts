import sys
import codecs

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"

print("Loading CELEX")
celex = {}
celex_verbs = {}
with codecs.open(tens_path + "other/DML.CD", "r", "utf-8") as f:
    DML = f.readlines()

with codecs.open(tens_path + "other/DPL.CD", "r", "utf-8") as f:                # do we need DPW so 'huisje' is actually matched in CELEX?
    DPL = f.readlines()

DMW = {}
with codecs.open(tens_path + "other/DMW.CD", "r", "utf-8") as f:                # We need it to determine wordclass, including diminuatives
    for line in f:
        l_list_dmw = line[:-1].split("\\")
        dmw_word = l_list_dmw[1]
        dmw_id = l_list_dmw[3]
        word_type = l_list_dmw[4]
        if dmw_word in DMW:
            if dmw_id in DMW[dmw_word]:
                DMW[dmw_word][dmw_id].append(word_type)
            else:
                DMW[dmw_word][dmw_id] = [word_type]
        else:
            DMW[dmw_word] = {dmw_id: [word_type]}

# get syllable count
syl_count = {}
with open(tens_path + "other/DPW.CD", "r") as f:
    for line in f:
        l_list = line[:-1].split("\\")
        word = l_list[1]
        syls = l_list[4].split("-")
        if syls == [""]:
            syl_count[word] = "NA"
        else:
            syl_count[word] = str(len(syls))


for line_num, line in enumerate(DML, 0):
    l_list = line[:-1].split("\\")
    word = l_list[1]
    lem_id = l_list[0]
    word_type = l_list[12]
    word_type = word_type[-2] if len(word_type) > 2 else ""
    if word not in celex:
        if word_type == "":
            if word in DMW:
                if lem_id in DMW[word]:
                    if ("de" in DMW[word][lem_id]) or ("e" in DMW[word][lem_id]):
                        word_type = "N"
                    elif "i" in DMW[word][lem_id]:
                        word_type = "V"
                    else:
                        word_type = ""
#                    word_type = "N" if ("de" in DMW[word][lem_id]) or ("e" in DMW[word][lem_id]) else ""
        if word_type == "N":
            l_list_dpl = DPL[line_num][:-1].split("\\")
            assert word == l_list_dpl[1]
            phon = l_list_dpl[3]                                                    # 3 instead of 6 so 'apenootje' gets the correct features
            parse = l_list[-8]                                                      # use different parse for words with different parses
    #        if len(parse) > 0:                                                      # exclude words for which compounding information is not present, or maybe not do this because it excludes a lot of words..
            compound = "+" in parse                                                 # check for compounds
            # extract final part of compound using DML
            if compound:
                fin_word = parse.split("+")[-1]
                compound = fin_word[:]
            if len(phon) > 2:
                celex[word] = {"phones": phon, "compound": compound}
        elif word_type == "V":  # and add_verbs:
            l_list_dpl = DPL[line_num][:-1].split("\\")
            assert word == l_list_dpl[1]
            phon = l_list_dpl[6]
            parse = l_list[-8]
            compound = "+" in parse
            if compound:
                fin_word = parse.split("+")[-1]
                compound = fin_word[:]
            if len(phon) > 2:
                celex_verbs[word] = {"phones": phon, "compound": compound}


inv_lines = []
with open(tens_path + "other/invar_probs.csv", "r") as f:
    inv_lines = [l[:-1] for l in f]

with open(tens_path + "other/invar_probs2.csv", "w") as f:
    for num, l in enumerate(inv_lines, 1):
        if num == 1:
            f.write(l + ",compound,n_syl\n")
        else:
            wrd = l.split(",")[0]
            if wrd in celex:
                comp_str = "+" if celex[wrd]["compound"] else "-"
                syl_str = syl_count[wrd]
            else:
                comp_str = "NA"
                syl_str = "NA"
            f.write(l + "," + comp_str + "," + syl_str + "\n")
