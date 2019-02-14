import sys
import codecs
import glob

tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"
ali_path = "/Volumes/tensusers/ltenbosch/KALDI_FA_out/CGN_expanded_lex_v3/" if sys.platform == "darwin" else "/vol/tensusers/ltenbosch/KALDI_FA_out/CGN_expanded_lex_v3/"
tz_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/home/timzee/Docs/"

phon_dict = {}
with open(tz_path + "KALDI-CGN_phones3.txt", "r") as f:
    for line in f:
        phon_dict[line.split(",")[0]] = line[:-1].split(",")[1]

with codecs.open(tens_path + "validation_data_time_from_ifa_pos.csv", "w", encoding="utf-8") as h:
    with codecs.open(tens_path + "validation_data_time_from_ifa.csv", "r", encoding="utf-8") as f:
        prev_sentence = ""
        phon_num = 0
        for l_num, line in enumerate(f, 1):
            print(l_num, line)
            if l_num == 1:
                h.write(line[:-1] + ",kaldi_pos\n")
            else:
                l_list = line[:-1].split(",")
                sentence = l_list[0]
                kaldi_lab = l_list[3]
                if sentence != prev_sentence:
                    ali_lines = []
                    phon_num = 0
                    glob_list = glob.glob(ali_path + "*" + sentence + "_*")
                    print(sentence, glob_list)
                    if len(glob_list) > 0:
                        with codecs.open(glob_list[0], "r", encoding="utf-8") as g:
                            ali_lines = g.readlines()
                if len(ali_lines) > 1:
                    if kaldi_lab != "NA":
                        phon_num += 1
                        if phon_num > len(ali_lines) - 1:
                            kaldi_pos = "NA"
                        else:
                            kaldi_pos_l = ali_lines[phon_num].split("\t")[6].split("_")
                            kaldi_pos = kaldi_pos_l[1] if len(kaldi_pos_l) > 1 else kaldi_pos_l[0]
                            if phon_num == 1:
                                if phon_dict[kaldi_pos_l[0]] != kaldi_lab:
                                    ali_lines = []
                        print(kaldi_lab, phon_num, kaldi_pos_l)
                else:
                    kaldi_pos = "NA"
                h.write(line[:-1] + "," + kaldi_pos + "\n")
                prev_sentence = sentence[:]
