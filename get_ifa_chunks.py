import sys
import os
import textgrid
import re
import tempfile
import codecs
import glob

tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"

tg_folder = "SLcorpus/Labels/sentences/"

tz_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/home/timzee/Docs/"

validation_files = []
with open(tens_path + "validation_files.txt", "r") as f:
    for line in f:
        validation_files.append(line.split("/")[-1].split("_")[0])

allowed_styles = ["FR", "FS", "FT", "FW", "VI", "PS", "VS", "VT", "VW"]

filepaths = []
for speaker in os.listdir(tens_path + tg_folder):
    if os.path.isdir(tens_path + tg_folder + speaker):
        for fn in os.listdir(tens_path + tg_folder + speaker + "/phoneme/"):
            for style in allowed_styles:
                if re.search(r'[0-9]{}[0-9]'.format(style), fn):  # and fn.split("_")[0] not in validation_files:
                    filepaths.append(tens_path + tg_folder + speaker + "/phoneme/" + fn)

phone_dict = {}
with open(tz_path + "KALDI-IFA_phones.txt", "r") as f:
    for line in f:
        phone_dict[re.sub(r'\n', '', line).split(",")[1]] = line.split(",")[0]

kaldi_phons = set(list(phone_dict.values()))
phon_freq = {}
for i in kaldi_phons:
    phon_freq[i] = 0

phone_dict2 = {}
with open(tz_path + "KALDI-DDP_phones.txt", "r") as f:
    for line in f:
        phone_dict2[re.sub(r'\n', '', line).split(",")[1]] = line.split(",")[0]

phone_dict2.pop("")

diph_labels = []
for subfolder in ["consonants/", "diphtongs/", "vowels/"]:
    for diph_file in os.listdir(tz_path + "diphones/" + subfolder):
        if diph_file[0] != ".":
            diph_lab = re.sub(r"[0123sw][ab]?.sd.lab$", '', diph_file)
            diph_labels.append(diph_lab)

phone_combos = {}
for ddp_diph in set(diph_labels):
    phon1, phon2 = ddp_diph.split("_")
    if (phon1 in phone_dict2) and (phon2 in phone_dict2):
        if phone_dict2[phon2] in ["@", "AU", "EI", "EU", "UI", "a", "e", "i", "o", "u", "y", "A", "E", "I", "O", "U"] and phone_dict2[phon1] in ["A", "E", "I", "O", "U"]:
            continue
        if phone_dict2[phon1] == "" or phone_dict2[phon2] == "":
            continue
        kaldi_diph = phone_dict2[phon1] + "+" + phone_dict2[phon2]
        phone_combos[kaldi_diph] = 0

# clusters with SIL need to be added
for phon1 in ["@", "AU", "EI", "EU", "UI", "a", "e", "i", "o", "u", "y", "N", "S", "f", "j", "k", "l", "m", "n", "p", "r", "s", "t", "w", "x"]:
    phone_combos[phon1 + "+SIL"] = 0

phons2 = list(phone_dict2.values())
phons2.remove("")
phons2.remove("N")
for phon2 in phons2:
    phone_combos["SIL+" + phon2] = 0

# Load Mario lexicon
kaldi_lex = {}
with open(tz_path + "lexicon_from_MARIO.txt", "r") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        kaldi_lex[entry] = pron

# Load diphone frequencies
diph_loadings = {}
with open(tens_path + "all_diphone_freqs.txt", "r") as f:
    for line in f:
        key, val = line[:-1].split(",")
        diph_loadings[key] = float(val)

diph_loadings = {key: (1 / val) for key, val in diph_loadings.items() if val != 0}


def createTemp(tg_path):
    print(tg_path)
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(codecs.open(tg_path, encoding="iso-8859-1").read().encode("utf-8"))
    tempf.flush()
    return tempf


def loadTextGrid(tg_path):
    tg = textgrid.TextGrid()
    with createTemp(tg_path) as tempf:
        tg.read(tempf.name, encoding="utf-8")
    return tg


def getDiphones(tg_path):
    tg = loadTextGrid(tg_path)
    phone_list = [re.sub(r"__.*", "", interval.mark).strip("=") for interval in tg[0]]
    phone_list2 = []
    for i in phone_list:
        if i in ["SIL", "", "*", "sil", "#", "[]"]:
            phone_list2.append(i)
        else:
            i_list = re.findall(r'[a-zAOEGINSYZ29@][:+~]?', i)
            phone_list2.extend(i_list)
    diphone_list = []
    for counter, phon in enumerate(phone_list2, 0):
        if phon not in phone_dict:
            continue
        phon_freq[phone_dict[phon]] += 1
        if len(phone_list2) != 1:
            if counter != len(phone_list2) - 1:
                next_phon = phone_list2[counter + 1]
                if next_phon in phone_dict:
                    diphone_list.append(phone_dict[phon] + "+" + phone_dict[next_phon])
    for diphones in diphone_list:
        if diphones in phone_combos:
            phone_combos[diphones] += 1
#        else:  # uncomment this statement if you just want frequencies for all occurring diphones in IFA
#            phone_combos[diphones] = 1


def getHandAnnot(tg_path):
    tg = loadTextGrid(tg_path)
    phone_list = [re.sub(r"__.*", "", interval.mark).strip("=") for interval in tg[0]]
    phone_list2 = []
    for i in phone_list:
        if i in ["SIL", "", "*", "sil", "#", "[]"]:
            phone_list2.append(i)
        else:
            i_list = re.findall(r'[a-zAOEGINSYZ29@][:+~]?', i)
            phone_list2.extend(i_list)
    start_time = tg[0][0].minTime
    end_time = tg[0][len(tg[0]) - 1].maxTime
    diphone_list = []
    for counter, phon in enumerate(phone_list2, 0):
        if phon not in phone_dict:
            continue
        if len(phone_list2) != 1:
            if counter != len(phone_list2) - 1:
                next_phon = phone_list2[counter + 1]
                if next_phon in phone_dict:
                    diphone_list.append(phone_dict[phon] + "+" + phone_dict[next_phon])
    total_diph_load = 0
    for diph in diphone_list:
        if diph in diph_loadings:
            total_diph_load += diph_loadings[diph]
    phone_list3 = []
    for i in phone_list2:
        if i in ["SIL", "", "*", "sil", "#", "[]"]:
            continue
        phone_list3.append(phone_dict[i])
    # Now get orthography
    ort_list = [re.sub(r"__.*", "", interval.mark).strip("=") for interval in tg.getFirst("TRANSLIT")]
    orthography = ""
    for ort in ort_list:
        if ort in ["#", "*"]:
            continue
        ort = re.sub(r"\*[^ ]*", "", ort.lower())
        ort = re.sub(r"(`|'')", "'", ort)
        ort = re.sub(r"-", " ", ort)
        ort = re.sub(r" [!?.,:;]", "", ort)
        orthography += ort
    return start_time, end_time, phone_list3, (total_diph_load / len(diphone_list)), orthography.strip(" ")


def getCanonicalKALDI(orthography):
    w_list = orthography.split(" ")
    while "" in w_list:
        w_list.remove("")
    phonlist = []
    for word in w_list:
        if word not in kaldi_lex:
            return None
        phonlist.extend(kaldi_lex[word].split(" "))
    return phonlist


def writeDiphoneFreqs():
    """Write list diphone frequencies"""
    for sentence_path in filepaths:
        getDiphones(sentence_path)
    with open(tens_path + "diphone_freqs.txt", "w") as f:
        for key in phone_combos:
            f.write(key + "," + str(phone_combos[key]) + "\n")


def selectSentences():
    ifa_list = []
    for sentence_path in filepaths:
        start, end, hand_annot, diph_load, ort = getHandAnnot(sentence_path)
        canon_annot = getCanonicalKALDI(ort)
        if not canon_annot:
            print("OOV detected")
            continue
        annot_diff = len(canon_annot) - len(hand_annot)
        num_red = annot_diff if annot_diff > 0 else 0
        sound_path = tens_path + "SLspeech/sentences/hm/" + sentence_path.split("/")[-3] + "/" + sentence_path.split("/")[-1].split("_")[0] + "_hm.wav"
        ifa_list.append((sound_path, start, end, ort, num_red, diph_load, (num_red + 1) * diph_load))
    with open(tens_path + "IFA_sentences.txt", "w") as f:
        for i in sorted(ifa_list, key=lambda score: score[-1], reverse=True):
            sp, start, end, ort, n_red, diph_load, score = i
            f.write(",".join([sp, str(start), str(end), ort, str(n_red), str(diph_load), str(score)]).encode("utf-8") + "\n")


def appendPercentages():
    with open(tens_path + "IFA_sentences_perc.txt", "w") as g:
        with open(tens_path + "IFA_sentences.txt") as f:
            for line in f:
                l_list = line[:-1].split(",")
                f_name = l_list[0].split("_")[0].split("/")[-1]
                sp_name = f_name[:4]
                glob_path = tens_path + tg_folder + sp_name + "/phoneme/" + f_name + "*"
                tg_path = glob.glob(glob_path)[0]
                getDiphones(tg_path)
                value_list = list(phone_combos.values())
                diph_perc_zero = 100 * (float(value_list.count(0)) / len(value_list))
                value_list2 = list(phon_freq.values())
                ph_perc_zero = 100 * (float(value_list2.count(0)) / len(value_list2))
#                ph_zero = {k: v for k, v in phon_freq.items() if v == 0}
#                print(ph_zero.keys())
                g.write(line[:-1] + "," + str(diph_perc_zero) + "," + str(ph_perc_zero) + "\n")


if __name__ == '__main__':
    appendPercentages()
