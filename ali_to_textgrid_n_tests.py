#!/usr/bin/env python2

import sys
import textgrid
import decimal

tz_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/home/timzee/Docs/"
tens_path = "/Volumes/tensusers/timzee/cgn/n_tests/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/n_tests/"

phon_dict = {}
with open(tz_path + "KALDI-CGN_phones3.txt", "r") as f:
    for line in f:
        phon_dict[line.split(",")[0]] = line[:-1].split(",")[1]

print("Loading core index")
core_index = []
with open(tens_path + "prep_o_core.txt", "r") as f:
    for chunk in f:
        chunk_l = chunk[:-1].split(",")
        core_index.append("_".join(chunk_l[0].split("/")) + "_" + chunk_l[-1] + "_" + chunk_l[2] + "_" + chunk_l[3] + ".ali")


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
    files_list = [line[:-1] for line in sys.stdin if line[:-1] in core_index]
    for file_n, fp in enumerate(files_list, 1):
        try:
            with open(fp, "r") as f:
                ali_lines = f.readlines()
        except ValueError:
            sys.exit()
        if len(ali_lines) != 1:  # ignore empty .ali files
            tg = processAli(ali_lines)
            if len(sys.argv) != 2:
                # construct output path from inputfile
                print file_n, fp
                fp_id, tier, start, end = fp.split("/")[-1][:-4].split("_")[2:]
                fp_sentence = start + "-" + end
                output_path = tens_path + "validation/o/" + fp_id + "/" + tier + "/"
            else:
                output_path = sys.argv[1]
                fp_sentence = fp[:-4]
#            print("Writing " + fp_sentence)
            with open(output_path + fp_sentence + "_KA2.aspex", "w") as f:
                tg.write(f, short=True)


if __name__ == '__main__':
    readWriteAli()
