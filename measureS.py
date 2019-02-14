import sys
import glob
import codecs
import re

tens_path = "/Volumes/tensusers/timzee/cgn/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/"

index_dict = {}


def measureS(ali_l, s_word_i):
    word_dict = {}
    ali_i = {}
    for ali_num, line in enumerate(ali_l, 1):
        line_list = line[:-1].split("\t")
        if ali_num == 1:
            for n_num, n in enumerate(line_list, 0):
                ali_i[n] = n_num
        else:
            start_t = float(line_list[ali_i["start"]])
            end_t = start_t + float(line_list[ali_i["dur"]])
            kal_lab = line_list[ali_i["phone"]]
            if kal_lab in ["SIL"]:
                continue
            phon, posi = kal_lab.split("_")
            if posi in ["B", "S"]:
                word_num = len(word_dict) + 1
                word_dict[word_num] = [(phon, start_t, end_t)]
            else:
                word_num = len(word_dict)
                word_dict[word_num].append((phon, start_t, end_t))
    s_label = word_dict[int(s_word_i)][-1][0]
    if s_label != "s":
        s_dur = "NA"
    else:
        s_dur = word_dict[int(s_word_i)][-1][2] - word_dict[int(s_word_i)][-1][1]
    return s_dur


def readWrite():
    with codecs.open(tens_path + "all_s_combined_dur.csv", "w", encoding="utf-8") as h:
        with codecs.open(tens_path + "all_s_combined.csv", encoding="utf-8") as f:
            for num, line in enumerate(f, 1):
                if num % 100 == 0:
                    print((num / 575823) * 100)
                line_list = line[:-1].split(",")
                if num == 1:
                    for col_num, name in enumerate(line_list, 0):
                        index_dict[name] = col_num
                    h.write(line[:-1] + ",s_dur\n")
                else:
                    f_path = line_list[index_dict["wav"]]
                    chunk_start = line_list[index_dict["chunk_start"]]
                    chunk_end = line_list[index_dict["chunk_end"]]
                    word_chunk_i = line_list[index_dict["word_chunk_i"]]
                    glob_list = glob.glob(tens_path + "KALDI_output/CGN_beam_5_100_v3/{0}_*_{1}_{2}.ali".format(re.sub(r"/", "_", f_path), chunk_start, chunk_end))
#                    print(f_path)
                    if len(glob_list) > 0:
                        print(glob_list[0])
                        with open(glob_list[0], "r") as g:
                            ali_lines = g.readlines()
                        if len(ali_lines) > 1:
                            s_length = measureS(ali_lines, word_chunk_i)
                            print(s_length)
                            h.write(line[:-1] + "," + str(s_length) + "\n")


if __name__ == '__main__':
    readWrite()
