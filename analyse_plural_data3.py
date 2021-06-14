import sys
import os
import codecs
import re
import unicodedata
import timbl
import multiprocessing
import random

random.seed(1)

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
output_path = tens_path + "timbl_files3/"

# all verbs in van dale
possible_verbs = [
    "aanval", "barbecue", "bijdrage", "bloed", "boete", "dam", "film", "flirt", "gal", "hengel", "hint", "hoeve", "kajak", "kap", "kenteken", "keten", "kik", "kit", "klem", "klik", "lade", "leg", "lobby", "maal", "match", "nagel", "orakel", "parel", "pass", "pik", "post", "ram", "regel", "ruzie", "sar", "sim", "speech", "sponsor", "sprint", "stel", "step", "stop", "storm", "tafel", "teken", "test", "tip", "trip", "twist", "type", "vinger", "vuur", "wapen", "werk", "wortel", "zijn", "zon"
]

forbidden_words = [
    # 2 meanings in Algemene Nederlandse Spraakkunst
    "bal", "cent", "curator", "harmonie", "hemel", "kolonie", "letter", "maat", "middel", "olie", "patroon", "plan", "portier", "rede", "reden", "strip", "stuk", "tafel", "test", "tip", "vader", "vizier", "wapen", "water", "wortel",

    # 2 meanings
    "apache",
    "band",      # die muziek maken
    "blad",      # bladen and bladeren are different words
    "brief",     # briefs
    "clip",
    "club",      # knots/disco
    "den",   # different lexemes + weird singular dede dt
    "dijbeen",   # different words
    "fee",
    "flap",
    "gang",      # leenwoord 'gengs'
    "gel",
    "gif",
    "horde",
    "inning",
    "klasse",    # klasse vs. klas
    "mul",
    "net",          # 2 meanings
    "sportster",    # different lexemes
    "stand",     # leenwoord 'stents'
    "step",      # step / steppe
    "zone",      # zonen is meervoud van zoon
    "pool",      # leenwoord 'poel'
    "pop",
    "post",
    "rede",  # plural reden confusable with singular reden
    "scoop",
    "sim",
    "sirene",
    "standaard",

    # male/female forms
    "advocate",
    "atlete",    # also exclude female forms for which the plural en is shared with male forms
    "echtgenote",
    "feminist",  # hoe weet je dat femisten niet mv van feministe is
    "spionne",

    "deur",      # 2 deurs
    "jaar",      # 2e jaars
    "stuk",      # 4 stuks
    "tand",      # 3 tands
    "maal",     # 2 maals eenheid
    "week",     # 2 weeks

    # measures/units
    "frank",     # valuta
    "mark",      # valuta
    "shilling",  # valuta
    "cent",      # valuta
    "karaat", "punt",  # maat/eenheid

    # mistakes in SUBTLEX
    "bank",         # bank's
    "bisschop",     # bisschop's
    "bos",          # bosssen
    "dokter"        # dokters/doktoren
    "ex",           # ex's
    "havik",        # havik's
    "klootzak",     # klootzak's
    "mark",         # mark's
    "partij",       # partij's
    "vrouw",        # vrouw's
    "wereld",       # wereld's
    "thee",         # thee's
    "schaduw",      # schaduw's
    "wereld",       # wereld's
    "werk",         # werk's
    "zatlap",       # zatlap's
    "das",          # das's
    "zus",          # zus's
    "hij",          # hij's
    "rijkelui",     # rijkelui's
    "mån",
    "dokter",       # doktoren
    "dynastie",     # dynastie<U+0091>n
    "zo",           # zo<U+0092>n
    "wéé",
    "àl",
    "ál",
    "gebeuren",     # gebeuren is treated as plural
    "koren",        # komrt
    "mutagen",      # nl'se woorde is mutageen
    "cocon",        # cocons cocoonen (zie SUBTLEX cocoonen niet een zelfst. naamw.)
    "l",

    # idiosyncratic plurals
    "kinder",    # andere 'speelse' betekenis, bovendien is het meervoud hier ers vs. eren
    "bankman",      # banklui
    "bootsman",     # bootslui
    "buitenman",    # buitenlui
    "jus",          # different lexemes
    "man",          # mannekes
    "persman",      # perslui
    "kantoorpik",   # kantoorpikkies
    "kop",          # koppies
    "kwaad",         # kwaaien
    "penny",         # pence
    "schip",    # schepen
    "kalf",     # kalveren

    # Adjectives
    "middelmaat",   # wij zijn middelmaats
    "engel",        # kan verward worden met de taal; wij zijn engels

    # derived from adjectives
    "blind", "laat", "dood", "lomp", "provinciaal", "sjiek", "sociaal", "rode", "naaste", "volbloed", "illegaal", "extreem",

    # -iteit
    "universiteit", "identiteit", "kwaliteit", "agressiviteit",

    # -heid
    "persoonlijkheid", "schoonheid", "snelheid", "grootheid", "beleefdheid",

    # -ing
    "dropping", "inning", "kidnapping", "shilling", "training", "afstandsbepaling", "banning", "bescherming", "bestrijding", "bewapening", "beëindiging", "conservering", "doorzetting", "functionering", "geheimhouding", "herkenning", "invoeging", "kalmering", "massavernietiging", "ontspanning", "opvoeding", "paring", "reclassering", "ringeling", "tekstverwerking", "uithouding", "uitlevering", "vermeerdering", "vernietiging", "veroudering", "verspreiding", "verwarming", "verwerking", "voedselvergiftiging", "voortplanting", "watervoorziening", "zelfbediening", "zelfverdediging", "zelfvernietiging", "aanmelding", "aanmoediging", "aantrekking", "achtervolging", "ademhaling", "afleiding", "aflevering", "afpersing", "afstoting", "bediening", "belegering", "beroving", "besturing", "betaling", "beveiliging", "bevolking", "bevoorrading", "beweging", "dreiging", "echtscheiding", "geleiding", "genezing", "herhaling", "investering", "inwijding", "lening", "mening", "misleiding", "omgeving", "ontmoeting", "ontsnapping", "ontsteking", "ontvoering", "ontwijking", "opsporing", "overleving", "programmering", "regering", "riolering", "samenzwering", "scheiding", "scheuring", "sluiting", "spanning", "straling", "toelating", "toewijding", "uitbetaling", "uitvoering", "verdediging", "verdoving", "vergelding", "verkenning", "verkiezing", "verkrachting", "verleiding", "verrassing", "verscheping", "verzekering", "waarneming", "waarschuwing", "zuivering",

    # plural token frequency < number of compounds with corresponding interfix
    "voortgang", "gebed", "geluid", "gevecht", "gezicht", "afstand", "bedrijf", "bruid", "bruiloft", "naam", "oorlog", "publiek", "seizoen", "toegang", "god", "arend", "district", "faillissement", "gedachte", "gevaar", "havik", "kanon", "lende", "lichaam", "linde", "moer", "neutron", "ongeluk", "uitgang", "vakbond", "verstand", "vierkant", "wereld", "groep", "rel", "zon",

    # English forms more frequent than Dutch in Subtlex-NL
    "ring", "stern", "big", "client", "palm", "room"
]


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
verb_deletion_list = []
read_mode = False
Lemma = ""
POS = ""
noun_dict = {}
verb_dict = {}
count_freq = 0
cum_freq = 0
with codecs.open(tens_path + "other/SUBTLEX-NL.master.txt", "r", "utf-8") as f:
    for line in f:
        if read_mode:
            Wordform, SubPOS, FREQcount = unicodedata.normalize("NFC", line.replace('\x92', "'"))[:-1].split("\t")[2:5]    # NFKD
            count_freq += int(FREQcount)
            if POS == "N":
                if 'ev,' in SubPOS:                                                 # include option for words like meisje
                    if Wordform[-2:] == "je" and 'dim' in SubPOS:    # for words like meisje (only dim) but will also create new entry for wagentje under wagen
                        if Wordform in noun_dict:
                            if 'ev' in noun_dict[Wordform]:
                                noun_dict[Wordform]['ev'] += int(FREQcount)
                            else:
                                noun_dict[Wordform]['ev'] = int(FREQcount)
                        else:
                            noun_dict[Wordform] = {'ev': int(FREQcount)}
                    elif Lemma == Wordform and 'basis' in SubPOS:                # beesten would count towards ev without Lemma == Wordform
                        if 'ev' in noun_dict[Lemma]:
                            noun_dict[Lemma]['ev'] += int(FREQcount)
                        else:
                            noun_dict[Lemma]['ev'] = int(FREQcount)
                elif 'mv,' in SubPOS:
                    if Wordform[-3:] == "jes" and 'dim' in SubPOS:    # for words like meisjes
                        if Wordform[:-1] in noun_dict:
                            if "S" in noun_dict[Wordform[:-1]]:
                                noun_dict[Wordform[:-1]]["S"] += int(FREQcount)
                            else:
                                noun_dict[Wordform[:-1]]["S"] = int(FREQcount)
                        else:
                            noun_dict[Wordform[:-1]] = {"S": int(FREQcount)}
                    elif 'basis' in SubPOS and Lemma != Wordform:
                        if Wordform[-2:] in ["en", "ën"] and (len(Wordform) - len(Lemma)) < 4:  # exclude kinderen and koeien but not bessen or assurantiën
                            if len(Wordform) > 3:
                                if Wordform[-3] == "i" and Lemma[-1] != "i" and Wordform[-2] != "ë":
                                    pl_type = "OTHER"
                                elif Wordform[-3] in ["d", "t"] and Lemma[-1] not in ["d", "t"] and (len(Wordform) - len(Lemma)) > 1:      # exclude lijkwa - lijkwaden
                                    if Wordform[-4] in frivative_voicing:
                                        alt_fric = frivative_voicing[Wordform[-4]]
                                    else:
                                        alt_fric = "NALALA"
                                    if Lemma[-3:] in [Wordform[-4] + Wordform[-2:], alt_fric + Wordform[-2:]]:  # these are verbs marked as nouns
                                        verb_deletion_list.append(Lemma)
                                        pl_type = ""
                                    elif Wordform[-3] == "d":                      # add spaden plural in spa lemma to spade lemma
                                        if Wordform[:-1] in noun_dict:      # remaining verbs will be excluden by celex
                                            if "EN" in noun_dict[Wordform[:-1]]:
                                                noun_dict[Wordform[:-1]]["EN"] += int(FREQcount)
                                            else:
                                                noun_dict[Wordform[:-1]]["EN"] = int(FREQcount)
                                        else:
                                            noun_dict[Wordform[:-1]] = {'ev': 0, "EN": int(FREQcount)}
                                    pl_type = ""
                                elif Lemma[-3:] == Wordform[-3:]:
                                    verb_deletion_list.append(Lemma)
                                    pl_type = ""
                                else:
                                    pl_type = "EN"
                            else:
                                pl_type = "EN"
                        elif Wordform[-1] == "s":  # exclude bosjes that has been tagged as mv,basis; and crisis that is pluralized as crises
                            if (len(Wordform) - len(Lemma)) == 1:
                                pl_type = "S"
                            elif (len(Wordform) - len(Lemma)) == 2 and Wordform[-2] == "'":
                                if Wordform[-3] not in ["s"]:                             # excludes zus's but nor ex's
                                    pl_type = "S"
                                else:
                                    pl_type = "OTHER"
                            elif (len(Wordform) - len(Lemma)) == 2 and Wordform[-2] == "e":     # take care of words like speeches
                                pl_type = "S"
                            elif ((len(Wordform) - len(Lemma)) == 2 or (len(Wordform) - len(Lemma)) == 3) and Wordform[-3:] == "des":
                                if Wordform[:-1] in noun_dict:      # add zijdes plural in zij lemma to zijde lemma
                                    if "S" in noun_dict[Wordform[:-1]]:
                                        noun_dict[Wordform[:-1]]["S"] += int(FREQcount)
                                    else:
                                        noun_dict[Wordform[:-1]]["S"] = int(FREQcount)
                                else:
                                    noun_dict[Wordform[:-1]] = {'ev': 0, "S": int(FREQcount)}
                                pl_type = ""
                            else:
                                if len(Wordform) > 3:
                                    if Wordform[-3:] == "jes":
                                        pl_type = ""
                                    elif Lemma == "giraf":
                                        pl_type = "S"
                                    else:
                                        pl_type = "OTHER"                                     # exclude mislabeled diminuatives as plurals
                                else:
                                    pl_type = "OTHER"
                        else:                                                       # add re restriction so that strategi n isn't a plural
                            if re.search(r'[a-z][a-z]', Wordform[-2:]):
                                pl_type = "OTHER"
                        if pl_type in noun_dict[Lemma]:                             # Wordform later vervangen door pl_type: 'en', 's', 'other'
                            noun_dict[Lemma][pl_type] += int(FREQcount)
                        else:
                            if pl_type != "":
                                noun_dict[Lemma][pl_type] = int(FREQcount)
            elif POS == "WW":                                                   # let's ignore past tense for now
                if SubPOS in ["pv,tgw,mv"]:
                    if Wordform == Lemma:
                        verb_dict[Lemma] += int(FREQcount)
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
            elif not re.search(r"[0-9@%'.?, ]", Lemma) and re.search(r"([eao]i|[qwrtpsdfghjklzxcvbnm])en$", Lemma) and POS == "WW" and len(Lemma) > 3:
                read_mode = True
                if Lemma not in verb_dict:
                    verb_dict[Lemma] = 0
                cum_freq = int(FREQcount)
                count_freq = 0
            else:
                continue

# remove verbs
for v in list(set(verb_deletion_list)):
    del noun_dict[v]

noun_dict = {k: noun_dict[k] for k in noun_dict if len(noun_dict[k]) > 1}

# remove forbidden words
for w in forbidden_words:
    if w in noun_dict:
        del noun_dict[w]

verb_dict = {k: verb_dict[k] for k in verb_dict if verb_dict[k] > 0}

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


def stressMarking(compound, cel):
    num_syl_in_word = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', cel[compound]["compound"]))
    stress_marked = False
    syllables_in_compound = cel[compound]["phones"].split("-")
    for s in syllables_in_compound[-num_syl_in_word:]:
        if "'" in s:
            stress_marked = True
    return stress_marked


def getCompoundStress(compound, word, cel):
    if not stressMarking(compound, cel):  # only search and replace if no stress in syllables of compound word
        if word in cel:
            if cel[word]["compound"] and cel[word]["compound"] not in compound_exceptions:
                getCompoundStress(word, cel[word]["compound"], cel)
            else:
                pass
            new_phones = cel[word]["phones"]
            if re.search(r"^[']*[fvszSZxG]", new_phones):   # check corresponderende stemhebbende / -loze fricatieven aan het begin van gezwel (x@-'zwEl) in woorden als ettergezwel (E-t@r-G@-zwEl)
                if new_phones[0] == "'":
                    new_phones_alt = "'" + frivative_voicing[new_phones[1]] + new_phones[2:]
                else:
                    new_phones_alt = frivative_voicing[new_phones[0]] + new_phones[1:]
                if re.search(re.escape(new_phones_alt.replace("'", "")) + "(?!.*" + re.escape(new_phones_alt.replace("'", "")) + ")", cel[compound]["phones"]):
                    if not re.search(re.escape(new_phones_alt) + "(?!.*" + re.escape(new_phones_alt) + ")", cel[compound]["phones"]):  # to check whether the phones have already been updated by another parent compound
                        cel[compound]["phones"] = re.sub(re.escape(new_phones_alt.replace("'", "")) + "(?!.*" + re.escape(new_phones_alt.replace("'", "")) + ")", new_phones_alt, cel[compound]["phones"])
                else:
                    if not re.search(re.escape(new_phones) + "(?!.*" + re.escape(new_phones) + ")", cel[compound]["phones"]):  # to check whether the phones have already been updated by another parent compound
                        cel[w]["phones"] = re.sub(re.escape(new_phones.replace("'", "")) + "(?!.*" + re.escape(new_phones.replace("'", "")) + ")", new_phones, cel[compound]["phones"])
            else:
                if not re.search(re.escape(new_phones) + "(?!.*" + re.escape(new_phones) + ")", cel[compound]["phones"]):  # to check whether the phones have already been updated by another parent compound
                    cel[compound]["phones"] = re.sub(re.escape(new_phones.replace("'", "")) + "(?!.*" + re.escape(new_phones.replace("'", "")) + ")", new_phones, cel[compound]["phones"])
        else:
            remove_keys.append(compound)
            return


# update compound phones and find phones in DPL and replace corresponding phones in phon already
remove_keys = []
for w in celex:
    if celex[w]["compound"] and celex[w]["compound"] not in compound_exceptions:    # only search and replace (if no stress in syllables of compound word, and) if compound is not in exceptions (i.e. if it is a true compound)
        getCompoundStress(w, celex[w]["compound"], celex)

for k in remove_keys:
    del celex[k]

celex["hersen"] = {'phones': "'hEr-s@n", 'compound': False}
celex["enveloppe"] = {'phones': "An-v@-'lOp", 'compound': False}

# add 'n' to words like heiden
for i in celex:
    if celex[i]["phones"][-1] == "@" and i[-1] == "n":
        celex[i]["phones"] += "n"

# if add_verbs:
remove_keys = []
for w in celex_verbs:
    if celex_verbs[w]["compound"] and celex_verbs[w]["compound"] not in compound_exceptions:    # only search and replace (if no stress in syllables of compound word, and) if compound is not in exceptions (i.e. if it is a true compound)
        getCompoundStress(w, celex_verbs[w]["compound"] + "en", celex_verbs)

for k in remove_keys:
    del celex_verbs[k]

dataset_invar = {}
dataset_var = {}
invar_compounds = {}
for lemma in noun_dict:
    lemma_str = strip_accents(lemma)
    if lemma_str in celex:
        if len(noun_dict[lemma]) == 2:                                          # only invariable plurals
            lem_l = list(noun_dict[lemma].keys())
            lem_l.remove('ev')
            dataset_invar[lemma] = {"features": celex[lemma_str]["phones"], "class": lem_l[0], "freq": noun_dict[lemma][lem_l[0]], "ev": noun_dict[lemma]["ev"]}
            if "compound" in celex[lemma_str]:
                if celex[lemma_str]["compound"] in noun_dict and celex[lemma_str]["compound"] in celex:
                    if "'" in celex[celex[lemma_str]["compound"]]["phones"] and celex[lemma_str]["compound"] not in compound_exceptions:
                        if celex[lemma_str]["compound"] not in invar_compounds:
                            invar_compounds[celex[lemma_str]["compound"]] = [lemma]
                        else:
                            invar_compounds[celex[lemma_str]["compound"]].append(lemma)
        else:                                                                   # for some reason does not include 'hersen'
            plurals = [(k, noun_dict[lemma][k]) for k in noun_dict[lemma] if k != 'ev']
            dataset_var[lemma] = {"features": celex[lemma_str]["phones"], "class": [], "freq": []}
            for pl in plurals:
                dataset_var[lemma]["class"].append(pl[0])
                dataset_var[lemma]["freq"].append(pl[1])
                dataset_var[lemma]["ev"] = noun_dict[lemma]["ev"]

# exclude possible verbs (not in noun_dict in case we want to include plural verbs in training)
for v in possible_verbs:
    if v in dataset_var:
        del dataset_var[v]
    if v in dataset_invar:
        del dataset_invar[v]
    if v in invar_compounds:
        del invar_compounds[v]

# import json
# with codecs.open(tens_path + "other/dataset_invar.json", "w") as h:
#     json.dump(dataset_invar, h)


# if add_verbs:
dataset_verb = {}
for lemma in verb_dict:
    lemma_str = strip_accents(lemma)
    if lemma_str in celex_verbs:
        dataset_verb[lemma] = {"features": celex_verbs[lemma_str]["phones"], "freq": verb_dict[lemma]}

print("Creating classifier")

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


# invar_keys = list(dataset_invar.keys())
# sample_size = int(len(invar_keys) / 10)
# test_set_keys = random.sample(invar_keys, sample_size)

# dataset_test = {}
# for k in test_set_keys:
#     dataset_test[k] = dataset_invar.pop(k)


def getFeatures(word, dataset, num_s):
    syllables = dataset[word]['features'].split("-")
    extra_syls = len(syllables) - num_s
    syllables = syllables[-num_s:] if extra_syls >= 0 else abs(extra_syls) * ["="] + syllables
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
            coda = re.search(r"[{}]+$".format("".join(medeklinkers)), syl)
            coda = coda.group() if coda else "="
            features.extend([onset, nucleus, coda])
    return features, stress


# up until here everything everything can be global (for multiprocessing)
def runTimbl(instances, metric, shared_lemmas, type_merge, num_syl, nn_k, dist_weight, add_var, add_invar, add_verbs, verb_class):
    add_var_lab = "_both" if add_var and add_invar else "_invar" if add_invar and not add_var else "_var"
    add_verb_lab = "_" + verb_class if add_verbs else ""
    type_merge_lab = "_merge" if instances == "type" and type_merge else ""
    shrd_lab = "" if shared_lemmas else "_noshare"
    model_name = "pl_{}_{}{}_{}syl_k{}_{}{}{}{}".format(instances, metric, type_merge_lab, str(num_syl), str(nn_k), dist_weight, add_var_lab, shrd_lab, add_verb_lab)
    classifier = timbl.TimblClassifier(model_name + ".master", "-m{}:I1 -k {} -d {} -G 0".format(metric, nn_k, dist_weight), dist=True)    # give unique name for multiprocessing

    dataset_invar2 = dataset_invar.copy()
    if not shared_lemmas:   # remove words from invar that share the final lemma with a word in var
        del_nouns = []
        for w in dataset_var:
            if w in invar_compounds:            # e.g. infectieziekte in invar; ziekte in var
                for n in invar_compounds[w]:
                    if n in dataset_invar2:
                        del_nouns.append(n)
                        del dataset_invar2[n]
            w_str = strip_accents(w)
            if "compound" in celex[w_str]:
                if celex[w_str]["compound"] in invar_compounds:     # e.g. infectieziekte in invar; huidziekte in var
                    for m in invar_compounds[celex[w_str]["compound"]]:
                        if m in dataset_invar2:
                            del_nouns.append(m)
                            del dataset_invar2[m]
                if celex[w_str]["compound"] in dataset_invar2:       # e.g. juf in invar; schooljuf in var
                    del_nouns.append(celex[w_str]["compound"])
                    del dataset_invar2[celex[w_str]["compound"]]

    invar_strings = []
    merge_feats = []
    for wrd in dataset_invar2:
        feat, strs = getFeatures(wrd, dataset_invar2, num_syl)
        pl_class = dataset_invar2[wrd]['class']
        compound_word_syls = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex[strip_accents(wrd)]["compound"])) if celex[strip_accents(wrd)]["compound"] else 0
        line = [wrd] + feat + strs + [wrd[-1]]
        # tokens or types?
        if instances == "token":
            for i in range(dataset_invar2[wrd]['freq']):
                classifier.append(tuple(line), pl_class)
                val_feat = classifier.validatefeatures(tuple(line))
                invar_strings.append(classifier.delimiter.join(val_feat) + (classifier.delimiter if not classifier.delimiter == '' else ' ') + pl_class)
        else:
            if type_merge:
                if line[1:] + [pl_class] not in merge_feats:
                    classifier.append(tuple(line), pl_class)
                    val_feat = classifier.validatefeatures(tuple(line))
                    invar_strings.append(classifier.delimiter.join(val_feat) + (classifier.delimiter if not classifier.delimiter == '' else ' ') + pl_class)
                    merge_feats.append(line[1:] + [pl_class])
            else:
                classifier.append(tuple(line), pl_class)
                val_feat = classifier.validatefeatures(tuple(line))
                invar_strings.append(classifier.delimiter.join(val_feat) + (classifier.delimiter if not classifier.delimiter == '' else ' ') + pl_class)

    if add_verbs:
        for wrd in dataset_verb:
            feat, strs = getFeatures(wrd, dataset_verb, num_syl)
#            pl_class = dataset_verb[wrd]['class']
            compound_word_syls = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex_verbs[strip_accents(wrd)]["compound"])) if celex_verbs[strip_accents(wrd)]["compound"] else 0
            line = [wrd] + feat + strs + [wrd[-1]]
            # tokens or types?
            if instances == "token":
                for i in range(dataset_verb[wrd]['freq']):
                    classifier.append(tuple(line), verb_class)
                    val_feat = classifier.validatefeatures(tuple(line))
                    invar_strings.append(classifier.delimiter.join(val_feat) + (classifier.delimiter if not classifier.delimiter == '' else ' ') + verb_class)
            else:
                if type_merge:
                    if line[1:] + [verb_class] not in merge_feats:
                        classifier.append(tuple(line), verb_class)
                        val_feat = classifier.validatefeatures(tuple(line))
                        invar_strings.append(classifier.delimiter.join(val_feat) + (classifier.delimiter if not classifier.delimiter == '' else ' ') + verb_class)
                        merge_feats.append(line[1:] + [verb_class])
                else:
                    classifier.append(tuple(line), verb_class)
                    val_feat = classifier.validatefeatures(tuple(line))
                    invar_strings.append(classifier.delimiter.join(val_feat) + (classifier.delimiter if not classifier.delimiter == '' else ' ') + verb_class)

    if os.path.exists(output_path + model_name + ".var"):
        os.remove(output_path + model_name + ".var")

    # create test file
    pl_rel_freqs = []
    if not add_var:
        for wrd in dataset_var:
            feat, strs = getFeatures(wrd, dataset_var, num_syl)
            compound_word_syls = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex[strip_accents(wrd)]["compound"])) if celex[strip_accents(wrd)]["compound"] else 0
            pl_class = max([(dataset_var[wrd]["freq"][lab_i], lab) for lab_i, lab in enumerate(dataset_var[wrd]["class"])])[1]
            line = [wrd] + feat + strs + [wrd[-1]]
            classifier.addinstance(model_name + ".var", tuple(line), pl_class)                 # give unique name for multiprocessing
            # data for analysis
            s = 0
            en = 0
            other = 0
            cum_freq = 0
            for i, cl in enumerate(dataset_var[wrd]['class'], 0):
                cum_freq += int(dataset_var[wrd]['freq'][i])
                if cl == "S":
                    s = int(dataset_var[wrd]['freq'][i])
                elif cl == "EN":
                    en = int(dataset_var[wrd]['freq'][i])
                else:
                    other = int(dataset_var[wrd]['freq'][i])
            line2 = [wrd, str(s), str(en), str(other), str(dataset_var[wrd]['ev'])] + [str(compound_word_syls)] + feat + strs
            pl_rel_freqs.append(line2)
        print("Train & test classifier")
        classifier.train()
    #    classifier.save()
        classifier.test(model_name + ".var")                                                   # give unique name for multiprocessing
        testfile = [i for i in classifier.readtestoutput()]
        # process probabilities
        compare_p_f = []
        for n_l, l in enumerate(testfile, 0):
            word = l[0][0]
            prob_strings = re.search(r'(?<={ ).*(?= })', l[0][-1]).group().split(", ")
            probs = {plural_prob.split(" ")[0]: float(plural_prob.split(" ")[1]) for plural_prob in prob_strings}
            for pl in ["EN", "S", "OTHER"]:
                if pl not in probs:
                    probs[pl] = 0
            if add_verbs and add_verb_lab == "VERB":
                p_s = probs["S"]
                p_en = probs["EN"]
                p_other = probs["OTHER"]
                probs["S"] = p_s / (p_s + p_en + p_other)
                probs["EN"] = p_en / (p_s + p_en + p_other)
                probs["OTHER"] = p_other / (p_s + p_en + p_other)
            l2 = pl_rel_freqs[n_l]
            assert word == l2[0]
            l3 = [word, str(probs["S"]), str(probs["EN"]), str(probs["OTHER"])] + l2[1:6] + l2[-num_syl - 6:-num_syl] + l2[-2:]
            compare_p_f.append(l3)
    else:
        testfile = {}
        # recreate training file everytime account for flushing mechanism
        for v_test in dataset_var:
            classifier = timbl.TimblClassifier(model_name + ".master", "-m{}:I1 -k {} -d {} -G 0".format(metric, nn_k, dist_weight), dist=True)
            if add_invar:
                classifier.instances.extend(invar_strings)
            if type_merge:
                if add_invar:
                    fresh_merge_l = merge_feats[:]
                else:
                    fresh_merge_l = []
            for v_train in dataset_var:
                if v_train != v_test:
                    feat, strs = getFeatures(v_train, dataset_var, num_syl)
                    line = [v_train] + feat + strs + [v_train[-1]]
                    if instances == "token":
                        for cl_i, pl_class in enumerate(dataset_var[v_train]['class'], 0):
                            for i in range(dataset_var[v_train]['freq'][cl_i]):
                                classifier.append(tuple(line), pl_class)
                    else:
                        pl_class = max([(dataset_var[v_train]["freq"][lab_i], lab) for lab_i, lab in enumerate(dataset_var[v_train]["class"])])[1]
                        if type_merge:
                            if line[1:] + [pl_class] not in fresh_merge_l:
                                classifier.append(tuple(line), pl_class)
                                fresh_merge_l.append(line[1:] + [pl_class])
                        else:
                            classifier.append(tuple(line), pl_class)
            classifier.train()
            feat, strs = getFeatures(v_test, dataset_var, num_syl)
            compound_word_syls = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex[strip_accents(v_test)]["compound"])) if celex[strip_accents(v_test)]["compound"] else 0
            feat2 = [v_test] + feat + strs + [v_test[-1]]
            # data for analysis
            s = 0
            en = 0
            other = 0
            cum_freq = 0
            for i, cl in enumerate(dataset_var[v_test]['class'], 0):
                cum_freq += int(dataset_var[v_test]['freq'][i])
                if cl == "S":
                    s = int(dataset_var[v_test]['freq'][i])
                elif cl == "EN":
                    en = int(dataset_var[v_test]['freq'][i])
                else:
                    other = int(dataset_var[v_test]['freq'][i])
            line2 = [v_test, str(s), str(en), str(other), str(dataset_var[v_test]['ev'])] + [str(compound_word_syls)] + feat + strs
            pl_rel_freqs.append(line2)
            classlabel, distribution, distance = classifier.classify(tuple(feat2))
            testfile[v_test] = distribution
            for pl in ["EN", "S", "OTHER"]:
                if pl not in testfile[v_test]:
                    testfile[v_test][pl] = 0
        # process probabilities
        compare_p_f = []
        for n_l, word in enumerate(testfile, 0):
            if add_verbs and add_verb_lab == "VERB":
                p_s = testfile[word]["S"]
                p_en = testfile[word]["EN"]
                p_other = testfile[word]["OTHER"]
                testfile[word]["S"] = p_s / (p_s + p_en + p_other)
                testfile[word]["EN"] = p_en / (p_s + p_en + p_other)
                testfile[word]["OTHER"] = p_other / (p_s + p_en + p_other)
            l2 = pl_rel_freqs[n_l]
            assert word == l2[0]
            l3 = [word, str(testfile[word]["S"]), str(testfile[word]["EN"]), str(testfile[word]["OTHER"])] + l2[1:6] + l2[-num_syl - 6:-num_syl] + l2[-2:]
            compare_p_f.append(l3)

    # classlabel, distribution, distance = classifier.classify(("administrateur", "n", "i", "=", "str", "a", "=", "t", "|", "r", "-", "-", "+", "r")) # DISTRIBUTION DOES NOT WORK

#    if not add_var:                                     # turn into test on hold out set, so we can compare all combos
#        loo_acc = str(classifier.leaveoneout())
#    else:
#        loo_acc = "NA"

#     for wrd in dataset_test:
#         feat, strs = getFeatures(wrd, dataset_test, num_syl)
#         compound_word_syls = len(re.findall(r'[euoa]+(?=[^euioa]*)|(?<!e)i(?=[^euioa])', celex[strip_accents(wrd)]["compound"])) if celex[strip_accents(wrd)]["compound"] else 0
#         pl_class = dataset_test[wrd]['class']
#         line = [wrd] + feat + strs + [wrd[-1]]
#         classifier.addinstance(model_name + ".test", tuple(line), pl_class)

#     acc = str(classifier.test(model_name + ".test"))
    acc = str(classifier.leaveoneout())

    if os.path.exists(output_path + model_name + ".var"):
        os.remove(output_path + model_name + ".var")
    if os.path.exists(output_path + model_name + ".test"):
        os.remove(output_path + model_name + ".test")
    if os.path.exists(output_path + model_name + ".master.out"):
        os.remove(output_path + model_name + ".master.out")
    if os.path.exists(output_path + model_name + ".master.train"):
        os.remove(output_path + model_name + ".master.train")

    with codecs.open(output_path + "p_f_{}_{}{}_{}syl_k{}_{}{}{}{}.csv".format(instances, metric, type_merge_lab, str(num_syl), str(nn_k), dist_weight, add_var_lab, shrd_lab, add_verb_lab), "w", "utf-8") as f:        # save in subfolder for grid_search
        f.write("word,p_s,p_en,p_other,f_s,f_en,f_other,f_ev,right_compound_syls,penult_onset,penult_nucleus,penult_coda,final_onset,final_nucleus,final_coda,penult_stress,final_stress,accuracy\n")
        for ll in compare_p_f:
            f.write(",".join(ll + [acc]) + "\n")


def gridSearch(instncs=["type", "token"], mtrc=["O", "M"], shrd_lems=[False, True], merge=[True, False], n_syll=[1, 2, 3, 4], nneigh_k=[1, 2, 3, 4, 5, 6, 7], d_weight=["Z", "ID"], ad_var=[False, True], ad_invar=[False, True], ad_verb=[False, True], verb_cl=["VERB", "EN"]):
    inst_list = []
    m_list = []
    sh_list = []
    merge_list = []
    syl_list = []
    k_list = []
    d_list = []
    ad_var_list = []
    ad_invar_list = []
    ad_verb_list = []
    verb_cl_list = []
    # count number of recource intensive experiments
    num_long = 0
    for i in instncs:
        for m in mtrc:
            if i == "type":
                for tm in merge:
                    for s in n_syll:
                        for k in nneigh_k:
                            for d in d_weight:
                                for va in ad_var:
                                    for inv in ad_invar:
                                        if va or inv:
                                            if inv and not va:
                                                for sh in shrd_lems:
                                                    for ve in ad_verb:
                                                        if ve:
                                                            for v_c in verb_cl:
                                                                inst_list.append(i)
                                                                m_list.append(m)
                                                                sh_list.append(sh)
                                                                merge_list.append(tm)
                                                                syl_list.append(s)
                                                                k_list.append(k)
                                                                d_list.append(d)
                                                                ad_var_list.append(va)
                                                                ad_invar_list.append(inv)
                                                                ad_verb_list.append(ve)
                                                                verb_cl_list.append(v_c)
                                                        else:
                                                            inst_list.append(i)
                                                            m_list.append(m)
                                                            sh_list.append(sh)
                                                            merge_list.append(tm)
                                                            syl_list.append(s)
                                                            k_list.append(k)
                                                            d_list.append(d)
                                                            ad_var_list.append(va)
                                                            ad_invar_list.append(inv)
                                                            ad_verb_list.append(ve)
                                                            verb_cl_list.append("VERB")
                                            else:
                                                for ve in ad_verb:
                                                    if ve:
                                                        for v_c in verb_cl:
                                                            inst_list.append(i)
                                                            m_list.append(m)
                                                            sh_list.append(True)
                                                            merge_list.append(tm)
                                                            syl_list.append(s)
                                                            k_list.append(k)
                                                            d_list.append(d)
                                                            ad_var_list.append(va)
                                                            ad_invar_list.append(inv)
                                                            ad_verb_list.append(ve)
                                                            verb_cl_list.append(v_c)
                                                    else:
                                                        inst_list.append(i)
                                                        m_list.append(m)
                                                        sh_list.append(True)
                                                        merge_list.append(tm)
                                                        syl_list.append(s)
                                                        k_list.append(k)
                                                        d_list.append(d)
                                                        ad_var_list.append(va)
                                                        ad_invar_list.append(inv)
                                                        ad_verb_list.append(ve)
                                                        verb_cl_list.append("VERB")
            else:
                for s in n_syll:
                    for k in nneigh_k:
                        for d in d_weight:
                            for va in ad_var:
                                for inv in ad_invar:
                                    if va or inv:
                                        if inv and not va:
                                            for sh in shrd_lems:
                                                for ve in ad_verb:
                                                    if ve:
                                                        for v_c in verb_cl:
                                                            if va:
                                                                num_long += 1
                                                            inst_list.append(i)
                                                            m_list.append(m)
                                                            sh_list.append(sh)
                                                            merge_list.append(False)
                                                            syl_list.append(s)
                                                            k_list.append(k)
                                                            d_list.append(d)
                                                            ad_var_list.append(va)
                                                            ad_invar_list.append(inv)
                                                            ad_verb_list.append(ve)
                                                            verb_cl_list.append(v_c)
                                                    else:
                                                        if va:
                                                            num_long += 1
                                                        inst_list.append(i)
                                                        m_list.append(m)
                                                        sh_list.append(sh)
                                                        merge_list.append(False)
                                                        syl_list.append(s)
                                                        k_list.append(k)
                                                        d_list.append(d)
                                                        ad_var_list.append(va)
                                                        ad_invar_list.append(inv)
                                                        ad_verb_list.append(ve)
                                                        verb_cl_list.append("VERB")
                                        else:
                                            for ve in ad_verb:
                                                if ve:
                                                    for v_c in verb_cl:
                                                        if va:
                                                            num_long += 1
                                                        inst_list.append(i)
                                                        m_list.append(m)
                                                        sh_list.append(True)
                                                        merge_list.append(False)
                                                        syl_list.append(s)
                                                        k_list.append(k)
                                                        d_list.append(d)
                                                        ad_var_list.append(va)
                                                        ad_invar_list.append(inv)
                                                        ad_verb_list.append(ve)
                                                        verb_cl_list.append(v_c)
                                                else:
                                                    if va:
                                                        num_long += 1
                                                    inst_list.append(i)
                                                    m_list.append(m)
                                                    sh_list.append(True)
                                                    merge_list.append(False)
                                                    syl_list.append(s)
                                                    k_list.append(k)
                                                    d_list.append(d)
                                                    ad_var_list.append(va)
                                                    ad_invar_list.append(inv)
                                                    ad_verb_list.append(ve)
                                                    verb_cl_list.append("VERB")
    return [*zip(inst_list, m_list, sh_list, merge_list, syl_list, k_list, d_list, ad_var_list, ad_invar_list, ad_verb_list, verb_cl_list)], num_long


grid_search = False

if grid_search:
    tasks, n_long = gridSearch(instncs=["type"], ad_invar=[True], mtrc=["O"], nneigh_k=[1, 2, 3, 4, 5], ad_verb=[False], shrd_lems=[True])
    with codecs.open(output_path + "grid_search.log", "w") as f:
        f.write("Total experiments: {}\nLong experiments: {}\n\n".format(len(tasks), n_long))
        f.write("num,file_n,instncs,mtrc,shrd_lems,merge,n_syll,nneigh_k,d_weight,ad_var,ad_invar,ad_verb,verb_cl\n")
        for num, t in enumerate(tasks, 1):
            instan, metr, shr, t_merge, n_s, nearn_k, dis_weigh, advar, adinvar, adverbs, verbclass = t
            var_lab = "_both" if advar and adinvar else "_invar" if adinvar and not advar else "_var"
            verb_lab = "_" + verbclass if adverbs else ""
            merge_lab = "_merge" if instan == "type" and t_merge else ""
            shared_lab = "" if shr else "_noshare"
            file_n = "p_f_{}_{}{}_{}syl_k{}_{}{}{}{}.csv".format(instan, metr, merge_lab, str(n_s), str(nearn_k), dis_weigh, var_lab, shared_lab, verb_lab)
            str_l = [str(i) for i in t]
            f.write(str(num) + "," + file_n + "," + ",".join(str_l) + "\n")
#    tasks, n_long = gridSearch(instncs=["type"], merge=[False], n_syll=[1, 2], nneigh_k=[1], d_weight=["Z"], ad_var=[False], ad_invar=[True], ad_verb=[False], verb_cl=["VERB"])
    p = multiprocessing.Pool(60)
    p.starmap(runTimbl, iterable=tasks)
else:
    inst = "type"      # type or token
    mtr = "O"
    shrd = False
    mrg = True
    n_syl = 3           # 2
    n_k = 4
    d_w = "ID"
    add_v = False
    add_i = True
    add_verb = False
    verb_cl = "EN"     # VERB or EN
    runTimbl(inst, mtr, shrd, mrg, n_syl, n_k, d_w, add_v, add_i, add_verb, verb_cl)
