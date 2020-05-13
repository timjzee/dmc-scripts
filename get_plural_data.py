import sys
import codecs
import re
import unicodedata

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"

instances = "token"

frivative_voicing = {
    "f": "v",
    "v": "f",
    "s": "z",
    "z": "s",
    "S": "Z",
    "Z": "S",
    "x": "G",
    "G": "x"
}

compound_exceptions = [
    "a",
    "aal",
    "aan",
    "aar",
    "aard",
    "aarster",
    "aat",
    "ade",
    "age",
    "air",
    "ament",
    "ant",
    "antie",
    "ares",
    "arij",
    "ateur",
    "atie",
    "ator",
    "atorium",
    "atuur",
    "biliteit",
    "der",
    "derij",
    "dij",
    "dom",
    "e",
    "egge",
    "eiteit",
    "el",
    "elaar",
    "eling",
    "ement",
    "en",
    "enaar",
    "enares",
    "endom",
    "enier",
    "enij",
    "enis",
    "enist",
    "ent",
    "entie",
    "er",
    "erd",
    "eres",
    "erie",
    "erij",
    "erik",
    "ernij",
    "ertje",
    "es",
    "ess",
    "esse",
    "eteit",
    "etje",
    "eur",
    "euse",
    "foon",
    "heid",
    "iaal",
    "iaat",
    "iciteit",
    "ie",
    "ieel",
    "ief",
    "iek",
    "ier",
    "igheid",
    "ij",
    "ijn",
    "in",
    "ing",
    "ionair",
    "ionisme",
    "isme",
    "ist",
    "iste",
    "iteit",
    "itie",
    "itor",
    "je",
    "ling",
    "loos",
    "nce",
    "nij",
    "nis",
    "oir",
    "oos",
    "otor",
    "pje",
    "s",
    "schap",
    "se",
    "sel",
    "sie",
    "st",
    "ste",
    "ster",
    "t",
    "te",
    "tenis",
    "tie",
    "tje",
    "ueel",
    "utie",
    "uur"
]


def strip_accents(s):
    return ''.join(c for c in unicodedata.normalize('NFD', s) if unicodedata.category(c) != 'Mn')


print("Loading SUBTLEX")
read_mode = False
Lemma = ""
noun_dict = {}
count_freq = 0
cum_freq = 0
with codecs.open(tens_path + "other/SUBTLEX-NL.master.txt", "r", "utf-8") as f:
    for line in f:
        if read_mode:
            Wordform, SubPOS, FREQcount = unicodedata.normalize("NFC", line.replace('\x92', "'"))[:-1].split("\t")[2:5]    # NFKD
            count_freq += int(FREQcount)
            if 'ev,' in SubPOS:                                                 # include option for words like meisje
                if Wordform[-2:] == "je" and 'dim' in SubPOS:    # for words like meisje (only dim) but will also create new entry for wagentje under wagen
                    if Wordform in noun_dict:
                        if 'ev' in noun_dict[Wordform]:
                            noun_dict[Wordform]['ev'] += int(FREQcount)
                        else:
                            noun_dict[Wordform]['ev'] = int(FREQcount)
                    else:
                        noun_dict[Wordform] = {'ev': int(FREQcount)}
                elif Lemma == Wordform and 'basis' in SubPOS:                   # beesten would count towards ev without Lemma == Wordform
                    if 'ev' in noun_dict[Lemma]:
                        noun_dict[Lemma]['ev'] += int(FREQcount)
                    else:
                        noun_dict[Lemma]['ev'] = int(FREQcount)
            elif 'mv,' in SubPOS:
#                if Lemma == Wordform and Wordform[-3:] == "jes" and 'dim' in SubPOS:    # for words like meisjes
                if Wordform[-3:] == "jes" and 'dim' in SubPOS:    # for words like meisjes
                    if Wordform[:-1] in noun_dict:
                        if "S" in noun_dict[Wordform[:-1]]:
                            noun_dict[Wordform[:-1]]["S"] += int(FREQcount)
                        else:
                            noun_dict[Wordform[:-1]]["S"] = int(FREQcount)
                    else:
                        noun_dict[Wordform[:-1]] = {"S": int(FREQcount)}
                elif 'basis' in SubPOS and Lemma != Wordform:
                    if Wordform[-2:] in ["en", "ën"] and (len(Wordform) - len(Lemma)) < 4:  # exclude kinderen and koeien but not bessen
                        if len(Wordform) > 3:
#                            if Wordform[-3:] == "ien":
#                                pl_type = "OTHER"
#                            else:
#                                pl_type = "EN"
                            if Wordform[-3] == "i" and Lemma[-1] != "i":
                                pl_type = "OTHER"
                            else:
                                pl_type = "EN"
                        else:
                            pl_type = "EN"
                    elif Wordform[-1] == "s" and (len(Wordform) - len(Lemma)) < 3:  # exclude bosjes that has been tagged as mv,basis
                        pl_type = "S"
                    else:                                                       # add re restriction so that strategi n isn't a plural
                        if re.search(r'[a-z][a-z]', Wordform[-2:]):
                            pl_type = "OTHER"
                    if pl_type in noun_dict[Lemma]:                             # Wordform later vervangen door pl_type: 'en', 's', 'other'
                        noun_dict[Lemma][pl_type] += int(FREQcount)
                    else:
                        noun_dict[Lemma][pl_type] = int(FREQcount)
            if count_freq == cum_freq:
                read_mode = False
        else:
            Lemma, POS, Wordform, SubPOS, FREQcount = unicodedata.normalize("NFC", line.replace('\x92', "'"))[:-1].split("\t")[:5]
#            if Lemma not in ["@", "%", "'"] and POS == "N":
            if not re.search(r"[0-9@%'., ]", Lemma) and POS == "N":
                read_mode = True
                if Lemma == "hersenen":
                    Lemma = "hersen"
                    noun_dict[Lemma] = {'ev': 0}
                elif Lemma == "hersens":
                    Lemma = "hersen"
                elif Lemma == "ideeën":
                    Lemma = "idee"
                else:
                    if Lemma not in noun_dict:
                        noun_dict[Lemma] = {'ev': 0}
                cum_freq = int(FREQcount)
                count_freq = 0
            else:
                continue

noun_dict = {k: noun_dict[k] for k in noun_dict if len(noun_dict[k]) > 1}

print("Loading CELEX")
celex = {}
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
                    word_type = "N" if ("de" in DMW[word][lem_id]) or ("e" in DMW[word][lem_id]) else ""
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


def stressMarking(compound):
    num_syl_in_word = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex[compound]["compound"]))
    stress_marked = False
    syllables_in_compound = celex[compound]["phones"].split("-")
    for s in syllables_in_compound[-num_syl_in_word:]:
        if "'" in s:
            stress_marked = True
    return stress_marked


def getCompoundStress(compound, word):
    if not stressMarking(compound):  # only search and replace if no stress in syllables of compound word
        if word in celex:
            if celex[word]["compound"] and celex[word]["compound"] not in compound_exceptions:
                getCompoundStress(word, celex[word]["compound"])
            else:
                pass
            new_phones = celex[word]["phones"]
            if re.search(r"^[']*[fvszSZxG]", new_phones):   # check corresponderende stemhebbende / -loze fricatieven aan het begin van gezwel (x@-'zwEl) in woorden als ettergezwel (E-t@r-G@-zwEl)
                if new_phones[0] == "'":
                    new_phones_alt = "'" + frivative_voicing[new_phones[1]] + new_phones[2:]
                else:
                    new_phones_alt = frivative_voicing[new_phones[0]] + new_phones[1:]
                if re.search(re.escape(new_phones_alt.replace("'", "")) + "(?!.*" + re.escape(new_phones_alt.replace("'", "")) + ")", celex[compound]["phones"]):
                    if not re.search(re.escape(new_phones_alt) + "(?!.*" + re.escape(new_phones_alt) + ")", celex[compound]["phones"]):  # to check whether the phones have already been updated by another parent compound
                        celex[compound]["phones"] = re.sub(re.escape(new_phones_alt.replace("'", "")) + "(?!.*" + re.escape(new_phones_alt.replace("'", "")) + ")", new_phones_alt, celex[compound]["phones"])
                else:
                    if not re.search(re.escape(new_phones) + "(?!.*" + re.escape(new_phones) + ")", celex[compound]["phones"]):  # to check whether the phones have already been updated by another parent compound
                        celex[w]["phones"] = re.sub(re.escape(new_phones.replace("'", "")) + "(?!.*" + re.escape(new_phones.replace("'", "")) + ")", new_phones, celex[compound]["phones"])
            else:
                if not re.search(re.escape(new_phones) + "(?!.*" + re.escape(new_phones) + ")", celex[compound]["phones"]):  # to check whether the phones have already been updated by another parent compound
                    celex[compound]["phones"] = re.sub(re.escape(new_phones.replace("'", "")) + "(?!.*" + re.escape(new_phones.replace("'", "")) + ")", new_phones, celex[compound]["phones"])
        else:
            remove_keys.append(compound)
            return


# update compound phones and find phones in DPL and replace corresponding phones in phon already
remove_keys = []
for w in celex:
    if celex[w]["compound"] and celex[w]["compound"] not in compound_exceptions:    # only search and replace (if no stress in syllables of compound word, and) if compound is not in exceptions (i.e. if it is a true compound)
        getCompoundStress(w, celex[w]["compound"])


for k in remove_keys:
    del celex[k]

celex["hersen"] = {'phones': "'hEr-s@n", 'compound': False}

dataset_invar = {}
dataset_var = {}
for lemma in noun_dict:
    lemma_str = strip_accents(lemma)
    if lemma_str in celex:
        if len(noun_dict[lemma]) == 2:                                          # only invariable plurals
            lem_l = list(noun_dict[lemma].keys())
            lem_l.remove('ev')
            dataset_invar[lemma] = {"features": celex[lemma_str]["phones"], "class": lem_l[0], "freq": noun_dict[lemma][lem_l[0]]}
        else:                                                                   # for some reason does not include 'hersen'
            plurals = [(k, noun_dict[lemma][k]) for k in noun_dict[lemma] if k != 'ev']
            dataset_var[lemma] = {"features": celex[lemma_str]["phones"], "class": [], "freq": []}
            for pl in plurals:
                dataset_var[lemma]["class"].append(pl[0])
                dataset_var[lemma]["freq"].append(pl[1])
                dataset_var[lemma]["ev"] = noun_dict[lemma]["ev"]

print("Creating file")
# save trainingfile
korte_vocalen = ["I", "E", "A", "O", "}"]
lange_vocalen = ["i", "y", "e", "|", "a", "o", "u"]
diftongen = ["K", "L", "M"]
schwa = ["@"]
leen_vocalen = [")", "*", "<", "!", "("]
klinkers = korte_vocalen + lange_vocalen + diftongen + schwa + leen_vocalen
occlusieven = ["p", "b", "t", "d", "k", "g"]
fricatieven = ["f", "v", "s", "z", "S", "Z", "x", "G", "h"]
nasalen = ["m", "n", "N"]
liquidae = ["l", "r"]
halfvocalen = ["j", "w"]
medeklinkers = occlusieven + fricatieven + nasalen + liquidae + halfvocalen


with codecs.open(tens_path + "other/pl_" + instances + ".master", "w", "utf-8") as f:
    for word in dataset_invar:
        syllables = dataset_invar[word]['features'].split("-")
        syllables = syllables[-2:] if len(syllables) > 1 else ["="] + syllables
        features = []
        stress = []
        for syl in syllables:
            if syl == "=":
                features.extend(["=", "=", "="])
                stress.append("-")
            else:
                # check stress
                if syl[0] == "'":
                    stress.append("+")
                    syl = syl[1:]
                else:
                    stress.append("-")
                # find onset, nucleus, and coda
                onset = re.search(r'^[{}]+'.format("".join(medeklinkers)), syl)
                onset = onset.group() if onset else "="
                nucleus = re.search(r'[{}]+'.format("".join(klinkers)), syl)
                nucleus = nucleus.group() if nucleus else "="
                coda = re.search(r'[{}]+$'.format("".join(medeklinkers)), syl)
                coda = coda.group() if coda else "="
                features.extend([onset, nucleus, coda])
        pl_class = [dataset_invar[word]['class']]
        compound_word_syls = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex[strip_accents(word)]["compound"])) if celex[strip_accents(word)]["compound"] else 0
        line = [word] + [str(compound_word_syls)] + features + stress + [word[-1]] + pl_class
        # tokens or types?
        if instances == "token":
            for i in range(dataset_invar[word]['freq']):
                f.write(",".join(line) + "\n")
        else:
            f.write(",".join(line) + "\n")

with codecs.open(tens_path + "other/pl_type_rel_freqs.csv", "w", "utf-8") as g:
    with codecs.open(tens_path + "other/pl_type.var", "w", "utf-8") as f:
        for word in dataset_var:
            syllables = dataset_var[word]['features'].split("-")
            syllables = syllables[-2:] if len(syllables) > 1 else ["="] + syllables
            features = []
            stress = []
            for syl in syllables:
                if syl == "=":
                    features.extend(["=", "=", "="])
                    stress.append("-")
                else:
                    # check stress
                    if syl[0] == "'":
                        stress.append("+")
                        syl = syl[1:]
                    else:
                        stress.append("-")
                    # find onset, nucleus, and coda
                    onset = re.search(r'^[{}]+'.format("".join(medeklinkers)), syl)
                    onset = onset.group() if onset else "="
                    nucleus = re.search(r'[{}]+'.format("".join(klinkers)), syl)
                    nucleus = nucleus.group() if nucleus else "="
                    coda = re.search(r'[{}]+$'.format("".join(medeklinkers)), syl)
                    coda = coda.group() if coda else "="
                    features.extend([onset, nucleus, coda])
            pl_class = [dataset_var[word]['class'][0]]
            compound_word_syls = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex[strip_accents(word)]["compound"])) if celex[strip_accents(word)]["compound"] else 0
            line = [word] + [str(compound_word_syls)] + features + stress + [word[-1]] + pl_class
            f.write(",".join(line) + "\n")
            # file for analysis
            s = 0
            en = 0
            other = 0
            cum_freq = 0
            for i, cl in enumerate(dataset_var[word]['class'], 0):
                cum_freq += int(dataset_var[word]['freq'][i])
                if cl == "S":
                    s = int(dataset_var[word]['freq'][i])
                elif cl == "EN":
                    en = int(dataset_var[word]['freq'][i])
                else:
                    other = int(dataset_var[word]['freq'][i])
            line2 = [word, str(s), str(en), str(other), str(dataset_var[word]['ev'])] + [str(compound_word_syls)] + features + stress
            g.write(",".join(line2) + "\n")
