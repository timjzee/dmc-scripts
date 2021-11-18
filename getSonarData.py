import sys
import re
from frog import Frog, FrogOptions

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
        syl_struc = [i.strip("[]") for i in l_list[5].split("[") if i != ""]
        if syls == [""]:
            celex[word] = ["NA", "NA", "NA", "NA", "NA"]
        else:
            num_syl = len(syls)
            for counter, syl in enumerate(syls, 1):
                if "'" in syl:
                    stress = counter
                    break
            if len(syl_struc) > 0:
                celex[word] = [str(num_syl), str(stress), syls[0].strip("'")[0], "@" in syls[0], str(syl_struc[0].count("V"))]
            else:
                celex[word] = [str(num_syl), str(stress), syls[0].strip("'")[0], "@" in syls[0], "NA"]

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


with open(f_path + "SonarVar_test.csv", "w") as g:
    g.write("s_plural,item,prosodic_break,next_stress,next_sound,next_vowel_length,ort\n")
    with open(f_path + "sonar_plurals2.csv", "r") as f:
        for line_num, i_line in enumerate(f, 1):
            print("Processing line " + str(line_num))
            i_list = i_line[:-1].split("\t")
            sentence = i_list[1].strip('" ')
            foll_context = i_list[2].strip('" ')
            for t in trials:
                grep_text = "(^{}|[ ']{})({}|{})(?=[ .,?!':;])".format(t[0].upper(), t[0], trials_dict[t]["en"][1:], trials_dict[t]["s"][1:])
                if re.search(grep_text, sentence):
                    line = re.sub(" & ", " en ", sentence)
                    frog_output = frog.process(line)
                    frog_output_clean = [i for i in frog_output if i["pos"] != "LET()"]
                    w_indices = []
                    for num, b in enumerate(frog_output, 0):
                        if b["text"].lower() in [trials_dict[t]["en"], trials_dict[t]["s"]] and b["pos"] == "N(soort,mv,basis)":
                            w_indices.append(num)
                    for w_index in w_indices:
                        s_plural = "1" if frog_output[w_index]["text"][-1] == "s" else "0"
                        clean_index = [w["index"] for w in frog_output_clean].index(frog_output[w_index]["index"])
                        if clean_index + 1 < len(frog_output_clean):
                            next_word = frog_output_clean[clean_index + 1]["text"].lower()
                        else:
                            context_frog_output = frog.process(re.sub(" & ", " en ", foll_context))
                            context_frog_output_clean = [i for i in context_frog_output if i["pos"] != "LET()"]
                            if len(context_frog_output_clean) > 0:
                                next_word = context_frog_output_clean[0]["text"].lower()
                            else:
                                next_word = None
                        print("next_word = ", next_word)
                        if next_word:
                            if next_word in celex:
                                stressed_syl, next_sound, next_syl_schwa, next_vowel_length = celex[next_word][1:]
                                next_stress = "1" if stressed_syl == "1" and not next_syl_schwa else "NA" if stressed_syl == "NA" else "0"
                            else:
                                # check multi-word unit
                                if "_" in next_word:
                                    first_word = next_word.split("_")[0]
                                    if first_word in celex:
                                        stressed_syl, next_sound, next_syl_schwa, next_vowel_length = celex[first_word][1:]
                                        next_stress = "1" if stressed_syl == "1" and not next_syl_schwa else "NA" if stressed_syl == "NA" else "0"
                                    else:
                                        next_stress = "NA"
                                        next_sound = "NA"
                                        next_vowel_length = "NA"
                                else:
                                    next_stress = "NA"
                                    next_sound = "NA"
                                    next_vowel_length = "NA"
                        else:
                            next_stress = "NA"
                            next_sound = "NA"
                            next_vowel_length = "NA"
                        if w_index + 1 < len(frog_output):
                            following_interp = frog_output[w_index + 1]["pos"] == "LET()"
                        else:
                            context_frog_output = frog.process(re.sub(" & ", " en ", foll_context))
                            if len(context_frog_output) > 0:
                                following_interp = context_frog_output[0]["pos"] == "LET()"
                            else:
                                following_interp = "NA"
                        prosodic_break = "NA" if following_interp == "NA" else "1" if following_interp else "0"
                        g.write(",".join([s_plural, t, prosodic_break, next_stress, next_sound, next_vowel_length, re.sub(",", "", line)]) + "\n")
