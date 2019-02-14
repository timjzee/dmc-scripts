#!/usr/bin/env python2

import sys
import textgrid
import decimal

tz_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/home/timzee/Docs/"
tens_path = "/Volumes/tensusers/timzee/IFAcorpus/SLcorpus/Labels/validation_tim2/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/SLcorpus/Labels/validation_tim2/"

phon_dict = {}
with open(tz_path + "KALDI-CGN_phones3.txt", "r") as f:
    for line in f:
        phon_dict[line.split(",")[0]] = line[:-1].split(",")[1]


def processAli(ali_ls):
    int_list = []
    for counter, line in enumerate(ali_ls, 0):
        if counter > 0:
            l_list = line[:-1].split("\t")
            phon_start = decimal.Decimal(l_list[4])
            phon_end = phon_start + decimal.Decimal(l_list[5])
            phon_lab = "SIL" if l_list[6] == "SIL" else l_list[6].split("_")[0]
            int_list.append((phon_start, phon_end, phon_lab))
    phon_tier = textgrid.IntervalTier(name="PHONEMES", minTime=0, maxTime=int_list[-1][1])
    for i in int_list:
        if i[0] == i[1]:
            continue
        phon_tier.add(i[0], i[1], phon_dict[i[2]])
    phon_textgrid = textgrid.TextGrid(name=None, minTime=0, maxTime=int_list[-1][1])
    phon_textgrid.append(phon_tier)
    return phon_textgrid


def readWriteAli():
    files_list = [line[:-1] for line in sys.stdin]
    for fp in files_list:
        try:
            with open(fp, "r") as f:
                ali_lines = f.readlines()
        except ValueError:
            sys.exit()
        if len(ali_lines) != 1:  # ignore empty .ali files
            tg = processAli(ali_lines)
            # construct output path from inputfile
            fp_speaker, fp_sentence = fp.split("/")[-1].split("_")[8:10]
            output_path = tens_path + fp_speaker + "/ASPEX/" if len(sys.argv) != 3 else sys.argv[2]
            print("Writing " + fp_sentence)
            with open(output_path + fp_sentence + "_KA.aspex", "w") as f:
                tg.write(f, short=True)


if __name__ == '__main__':
    readWriteAli()
