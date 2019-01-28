import sys
import os
import textgrid
import re
import tempfile
import random
import codecs

tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"

tg_folder = "SLcorpus/Labels/sentences/"

tz_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/vol/timzee/Docs/"

validation_files = []
with open(tens_path + "validation_files.txt", "r") as f:
    for line in f:
        validation_files.append(line.split("/")[-1].split("_")[0])

allowed_styles = ["FR", "FS", "FT", "FW", "VI", "VS", "VT", "VW"]

filepaths = []
for speaker in os.listdir(tens_path + tg_folder):
    if os.path.isdir(tens_path + tg_folder + speaker):
        for fn in os.listdir(tens_path + tg_folder + speaker + "/ASPEX/"):
            for style in allowed_styles:
                if re.search(r'[0-9]{}[0-9]'.format(style), fn) and fn.split("_")[0] not in validation_files:
                    filepaths.append(tens_path + tg_folder + speaker + "/ASPEX/" + fn)

phone_dict = {}
with open(tz_path + "KALDI-ASPEX_phones.txt", "r") as f:
    for line in f:
        phone_dict[re.sub(r'\n', '', line).split(",")[1]] = line.split(",")[0]

phone_combos = {}
for i in set(phone_dict.values()):
    for j in set(phone_dict.values()):
        phone_combos[i + "+" + j] = 0


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
    phone_list = [str(interval.mark) for interval in tg[0]]
    start_time = tg[0][0].minTime
    end_time = tg[0][len(tg[0]) - 1].maxTime
    diphone_list = []
    for counter, phon in enumerate(phone_list, 0):
        if phon not in phone_dict:
            continue
        if len(phone_list) != 1:
            if counter != len(phone_list) - 1:
                next_phon = phone_list[counter + 1]
                if next_phon in phone_dict:
                    diphone_list.append(phone_dict[phon] + "+" + phone_dict[next_phon])
    for diphones in diphone_list:
        phone_combos[diphones] += 1
    return start_time, end_time


def getOrthography(tg_path):
    tg_list = tg_path.split("/")
    translit_path = "/".join(tg_list[:9]) + "/translit/" + tg_list[-1].split("_")[0] + ".translit"
    tg = loadTextGrid(translit_path)
    orthography = re.sub(r' __[0-9A-Z]+$', '', tg[0][1].mark)
    return orthography


def writeFiles():
    """Write list of 'chunks' and csv of diphone frequencies"""
    sample_list = random.sample(filepaths, 500 - len(validation_files))
    ifa_list = []
    for sentence_path in sample_list:
        start, end = getDiphones(sentence_path)
        ort = getOrthography(sentence_path)
        sound_path = tens_path + "SLspeech/sentences/hm/" + sentence_path.split("/")[-3] + sentence_path.split("/")[-1].split("_")[0] + "_hm.aifc"
        ifa_list.append(",".join([sound_path, str(start), str(end), ort]))
    with open(tens_path + "IFA_sentences.txt", "w") as f:
        for i in ifa_list:
            f.write(i.encode("utf-8") + "\n")


if __name__ == '__main__':
    writeFiles()
