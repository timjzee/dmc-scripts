import sys
import re
import subprocess
from frog import Frog, FrogOptions
from lxml import etree

frog = Frog(FrogOptions(parser=True))

f_path = "/Volumes/tensusers/timzee/other/" if sys.platform == "darwin" else "/vol/tensusers/timzee/other/"

trials = ["administrateur", "admiraal", "alarm", "balkon", "ballon", "bar", "baron", "bretel", "broer", "cabriolet", "champignon", "commandant", "compagnie", "crediteur", "dessert", "directeur", "donateur", "doorn", "duel", "dynastie", "epidemie", "expert", "galerie", "gazon", "gel", "generaal", "genie", "hoorn", "idee", "interieur", "journaal", "kanaal", "kapitein", "kopie", "luitenant", "magnetron", "majoor", "meneer", "mevrouw", "microfoon", "militair", "miljonair", "model", "monarch", "monogram", "monteur", "mortier", "officier", "perron", "pion", "pistool", "protocol", "redacteur", "regisseur", "reptiel", "residu", "restaurant", "saxofoon", "sergeant", "sjaal", "strategie", "telegram", "theorie", "trofee", "vampier"]

print("Loading CELEX")
celex = {}
with open(f_path + "DPW3.CD", "r") as f:
    for line in f:
        l_list = line[:-1].split("\\")
        word = l_list[1]
        syls = l_list[4].split("-")
        if syls == [""]:
            celex[word] = ["NA", "NA", "NA", "NA"]
        else:
            num_syl = len(syls)
            for counter, syl in enumerate(syls, 1):
                if "'" in syl:
                    stress = counter
                    break
            celex[word] = [str(num_syl), str(stress), syls[0].strip("'")[0], "@" in syls[0]]

double_vowels = ["ee", "uu", "oo", "aa"]
short_vowels = ["e", "u", "i", "o", "a"]

trials_dict = {}
for t in trials:
    trials_dict[t] = {}
    if t[-3:-1] in double_vowels:
        en_plural = t[:-2] + t[-1] + "en"
    elif t[-2] in short_vowels and t[-3] not in short_vowels and t[-1] not in short_vowels + ["w"]:
        en_plural = t + t[-1] + "en"
    elif t[-2:] in ["ee", "ie"]:
        en_plural = t + "ën"
    else:
        en_plural = t + "en"
    trials_dict[t]["en"] = en_plural
    if t[-1] in ["u", "o", "a"] and t[-2] not in short_vowels:
        s_plural = t + "'s"
    else:
        s_plural = t + "s"
    trials_dict[t]["s"] = s_plural


def getSyntax(wrd, tags, pw_index):
    pau = etree.Element("pau", ref="default", s="1")
    for counter, tag in enumerate(tags, 0):
        pw = etree.Element("pw", ref="default", w=tag["text"], pos=tag["pos"], lem=tag["lemma"], dep=tag["dep"], depindex=tag["depindex"], origindex=tag["index"], mwu="1" if "_" in tag["text"] else "0")
        pau.append(pw)
        if tag["lemma"] == wrd:
            pw_index = counter
    if not pw_index:
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
    pau_children = list(pau)
    pw = pau_children[pw_index]
    # ignore words that are part of multi-word-units
    if pw.attrib["mwu"] == "1":
        return ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
    # Get dependency grammar features
    # get closest incoming and outgoing link to the right
    dep_index_out = int(pw.attrib["depindex"])
    dep_out = pw.attrib["dep"]
    out_tup = (dep_index_out, dep_out, True)
    origindex = pw.attrib["origindex"]
    dep_index_in = 0
    dep_in = ""
    for pw_right in pau_children[pw_index:]:
        if pw_right.attrib["depindex"] == origindex:
            dep_index_in = int(pw_right.attrib["origindex"])
            dep_in = pw_right.attrib["dep"]
            break
    in_tup = (dep_index_in, dep_in, False)
    if dep_index_in and dep_index_out > int(origindex):  # if both incoming and outgoing right-adjacent links exist
        f1_index, f1, outgoing = min(out_tup, in_tup)
    else:
        if dep_index_in:  # only incoming
            f1 = dep_in
            f1_index = dep_index_in
            outgoing = False
        elif dep_index_out > int(origindex):  # only outgoing
            f1 = dep_out
            f1_index = dep_index_out
            outgoing = True
        else:
            f1 = "end"
            f1_index = int(origindex) + 1
            outgoing = True
    # now check for right-going and left-going links above closest right-adjacent link
    f2 = 0
    left_enc1 = 1000
    left_enc2 = 1000
    mwu = False
    prev_origindex = -1
    for pw_left in pau_children[:pw_index]:  # right-going links from preceding words
        mwu = pw_left.attrib["mwu"] == "1"
        if int(pw_left.attrib["depindex"]) >= f1_index:
            if mwu and prev_origindex == int(pw_left.attrib["origindex"]):  # ignore links from non-initial words in mwus
                pass
            else:
                f2 += 1
        # Number of words enclosed by shortest left-adjacent link
        if pw_left.attrib["depindex"] == origindex:
            left_enc1 = pw_index - pau_children.index(pw_left) - 1
            left_enc1 = 0 if left_enc1 < 0 else left_enc1
        if int(pw_left.attrib["origindex"]) == dep_index_out:
            left_enc2 = pw_index - pau_children.index(pw_left) - 1
            left_enc2 = 0 if left_enc2 < 0 else left_enc2
        prev_origindex = int(pw_left.attrib["origindex"])
    if left_enc1 == left_enc2 == 1000:
        f3 = 0
    else:
        f3 = min(left_enc1, left_enc2)
#    print("f2 A", f2)
    if not outgoing:
        if dep_index_out > f1_index:  # right-going link from current word
            f2 += 1
#    print("f2 B", f2)
    num_link = 0
    num_right_link = 0
    f4 = 0
    mwu = False
    prev_origindex = -1
    for pw_right in pau_children[pw_index + 1:]:
        mwu = pw_right.attrib["mwu"] == "1"
        if (int(pw_right.attrib["depindex"]) <= int(origindex)) and (int(pw_right.attrib["depindex"]) != 0):  # left-going links from current and subsequent words
            num_link += 1
            if mwu and prev_origindex == int(pw_right.attrib["origindex"]):  # ignore links from non-initial words in mwus
                pass
            else:
                if not outgoing:
                    if num_link > 1:
                        f2 += 1
                else:
                    f2 += 1
        # Number of words enclosed by shortest right-adjacent link
        if not outgoing:
            if int(pw_right.attrib["depindex"]) == int(origindex) and num_link == 1:
                right_enc = pau_children.index(pw_right) - pw_index - 1
                f4 = right_enc if right_enc >= 0 else 0
        elif outgoing and f1 != "end":
            if int(pw_right.attrib["origindex"]) == f1_index:
                num_right_link += 1
                if num_right_link == 1:
                    right_enc = pau_children.index(pw_right) - pw_index - 1
                    f4 = right_enc if right_enc >= 0 else 0
        else:
            f4 = 0
        prev_origindex = int(pw_right.attrib["origindex"])
#    print("f2 C", f2)
    # Number of links going left/right from current word
    if dep_index_out == 0:
        f5 = 0
        f6 = 0
    elif dep_index_out > int(origindex):
        f5 = 0
        f6 = 1
    else:
        f5 = 1
        f6 = 0
    # Number of links going left/right from next word
    if pw_index + 1 == len(pau_children):
        f7 = 0
        f8 = 0
    else:
        next_pw = pau_children[pw_index + 1]
        dep_index_out_next = int(next_pw.attrib["depindex"])
        origindex_next = int(next_pw.attrib["origindex"])
        if dep_index_out_next == 0:
            f7 = 0
            f8 = 0
        elif dep_index_out_next > origindex_next:
            f7 = 0
            f8 = 1
        else:
            f7 = 1
            f8 = 0
    return [str(feat) for feat in [f1, f2, f3, f4, f5, f6, f7, f8]]


with open(f_path + "OSvar.csv", "w") as g:
    g.write("s_plural,item,next_stress,next_sound,f1,f2,f3,f4,f5,f6,f7,f8,ort\n")
    for t in trials:
        print("Processing " + t)
        grep_text = "(^{}|[ ']{})({}|{})(?=[ .,?!':;])".format(t[0].upper(), t[0], trials_dict[t]["en"][1:], trials_dict[t]["s"][1:])
        w_lines = subprocess.check_output(["grep", "-P", grep_text, f_path + "OpenSubtitles2018_nl_raw.txt"], universal_newlines=True)
        w_lines_list = w_lines.split("\n")
        w_lines_list = list(set(w_lines_list))
        w_lines_list = [i for i in w_lines_list if i != ""]
        for line in w_lines_list:
            line = re.sub(" & ", " en ", line)
            frog_output = frog.process(line)
            print(line)
            frog_output_clean = [i for i in frog_output if i["pos"] != "LET()"]
            lemmas = [i["lemma"] for i in frog_output_clean]
            if t not in lemmas:
                continue
            # get indices of matching words
            w_indices = []
            for num, b in enumerate(frog_output_clean, 0):
                if b["text"].lower() in [trials_dict[t]["en"], trials_dict[t]["s"]] and b["pos"] == "N(soort,mv,basis)":
                    w_indices.append(num)
            for w_index in w_indices:
                s_plural = "1" if frog_output_clean[w_index]["text"][-1] == "s" else "0"
                if w_index + 1 < len(frog_output_clean):
                    next_word = frog_output_clean[w_index + 1]["text"].lower()
                    if next_word in celex:
                        stressed_syl, next_sound, next_syl_schwa = celex[next_word][1:]
                        next_stress = "1" if stressed_syl == "1" and not next_syl_schwa else "NA" if stressed_syl == "NA" else "0"
                    else:
                        next_stress = "NA"
                        next_sound = "NA"
                    following_interp = frog_output[int(frog_output_clean[w_index]["index"])]["pos"] == "LET()"
                    next_stress = "0" if following_interp else next_stress
                else:
                    next_stress = "0"
                    next_sound = "EOS"
                syntax_feat = getSyntax(t, frog_output_clean, w_index)
                g.write(",".join([s_plural, t, next_stress, next_sound] + syntax_feat + [re.sub(",", "", line)]) + "\n")
