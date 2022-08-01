import sys
import re
from frog import Frog, FrogOptions
import multiprocessing

frog = Frog(FrogOptions(parser=True, numThreads=b"1"))

f_path = "/Volumes/tensusers/timzee/other/" if sys.platform == "darwin" else "/vol/tensusers/timzee/other/"

trials = ['vlies', 'cadeau', 'recept', 'talent', 'bonbon', 'hypocriet', 'auteur', 'beha', 'regime', 'debat', 'rabbijn', 'woestijn', 'abonnee', 'atelier', 'bataljon', 'matador', 'dialect', 'horoscoop', 'kapitaal', 'paragraaf', 'patholoog']

print("Loading CELEX")
celex = {}
with open(f_path + "DPW3.CD", "r") as f:
    for line in f:
        l_list = line[:-1].split("\\")
        word = l_list[1]
        syls = l_list[4].split("-")
        syl_struc = [i.strip("[]") for i in l_list[5].split("[") if i != ""]
        if syls == [""]:
            celex[word] = ["NA", "NA", "NA", "NA", "NA", "NA"]
        else:
            num_syl = len(syls)
            for counter, syl in enumerate(syls, 1):
                if "'" in syl:
                    stress = counter
                    break
            if len(syl_struc) > 0:
                celex[word] = [str(num_syl), str(stress), syls[0].strip("'")[0], "@" in syls[0], str(syl_struc[0].count("V")), str(len(re.search('^C*', syl_struc[0]).group()))]
            else:
                celex[word] = [str(num_syl), str(stress), syls[0].strip("'")[0], "@" in syls[0], "NA", "NA"]

double_vowels = ["ee", "uu", "oo", "aa"]
short_vowels = ["e", "u", "i", "o", "a"]

trials_dict = {}
for t in trials:
    trials_dict[t] = {}
    if t == "vlies":
        en_plural = "vliezen"
    elif t[-3:-1] in double_vowels:
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


input_data = []
with open(f_path + "sonar_invars2.csv", "r") as f:
    for num, i in enumerate(f, 1):
        input_data.append([i, num])


def getVariables(i_pair):
    i_line, i_num = i_pair
    print("Processing line " + str(i_num))
    i_list = i_line[:-1].split("\t")
    sentence = i_list[1].strip('" ')
    foll_context = i_list[2].strip('" ')
    for t in trials:
        grep_text = "(^{}|[ ']{})({}|{})(?=[ .,?!':;])".format(t[0].upper(), t[0], trials_dict[t]["en"][1:], trials_dict[t]["s"][1:])
        if re.search(grep_text, sentence):
            line = re.sub(" & ", " en ", sentence)
            frog_output = frog.process(line)
            frog_output_clean = [i for i in frog_output if i["pos"] != "LET()"]
            n_foreign_tags = len([i for i in frog_output_clean if i["pos"] == "SPEC(vreemd)"])
            if n_foreign_tags > 0:
                return None
            w_indices = []
            for num, b in enumerate(frog_output, 0):
                if b["text"].lower() in [trials_dict[t]["en"], trials_dict[t]["s"]] and b["pos"] == "N(soort,mv,basis)":
                    w_indices.append(num)
            for w_index in w_indices:
                s_plural = "1" if frog_output[w_index]["text"][-1] == "s" else "0"
                clean_index = [w["index"] for w in frog_output_clean].index(frog_output[w_index]["index"])
                if clean_index + 1 < len(frog_output_clean):
                    next_word = frog_output_clean[clean_index + 1]["text"].lower()
                    next_lemma = frog_output_clean[clean_index + 1]["lemma"].lower()
                    next_POS = frog_output_clean[clean_index + 1]["pos"]
                else:
                    context_frog_output = frog.process(re.sub(" & ", " en ", foll_context))
                    context_frog_output_clean = [i for i in context_frog_output if i["pos"] != "LET()"]
                    if len(context_frog_output_clean) > 0:
                        next_word = context_frog_output_clean[0]["text"].lower()
                        next_lemma = context_frog_output_clean[0]["lemma"].lower()
                        next_POS = context_frog_output_clean[0]["pos"]
                    else:
                        next_word = None
                        next_lemma = "NA"
                        next_POS = "NA"
                if next_word:
                    if next_word in celex:
                        stressed_syl, next_sound, next_syl_schwa, next_vowel_length, n_next_cons = celex[next_word][1:]
                        next_stress = "1" if stressed_syl == "1" and not next_syl_schwa else "NA" if stressed_syl == "NA" else "0"
                    else:
                        # check multi-word unit
                        if "_" in next_word:
                            first_word = next_word.split("_")[0]
                            if first_word in celex:
                                stressed_syl, next_sound, next_syl_schwa, next_vowel_length, n_next_cons = celex[first_word][1:]
                                next_stress = "1" if stressed_syl == "1" and not next_syl_schwa else "NA" if stressed_syl == "NA" else "0"
                            else:
                                next_stress = "NA"
                                next_sound = "NA"
                                next_vowel_length = "NA"
                                n_next_cons = "NA"
                        else:
                            next_stress = "NA"
                            next_sound = "NA"
                            next_vowel_length = "NA"
                            n_next_cons = "NA"
                else:
                    next_stress = "NA"
                    next_sound = "NA"
                    next_vowel_length = "NA"
                    n_next_cons = "NA"
                if w_index + 1 < len(frog_output):
                    following_interp = frog_output[w_index + 1]["pos"] == "LET()"
                else:
                    context_frog_output = frog.process(re.sub(" & ", " en ", foll_context))
                    if len(context_frog_output) > 0:
                        following_interp = context_frog_output[0]["pos"] == "LET()"
                    else:
                        following_interp = "NA"
                prosodic_break = "NA" if following_interp == "NA" else "1" if following_interp else "0"
                return ",".join([s_plural, t, prosodic_break, next_stress, next_sound, next_vowel_length, n_next_cons, re.sub(",", "_", next_POS), next_lemma, re.sub(",", "", line)]) + "\n"


p = multiprocessing.Pool(60)

result_list = p.map(getVariables, input_data)

result_list = [line for line in result_list if line]


print("Writing output file...")
with open(f_path + "SonarInvar.csv", "w") as g:
    g.write("s_plural,item,prosodic_break,next_stress,next_sound,next_vowel_length,n_next_cons,next_POS,next_lemma,ort\n")
    for line in result_list:
        g.write(line)
