import panphon.distance
import codecs
import sys
import re

tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"
tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
gs_num = "gs20"

kaldi2ipa = {}
cgn2ipa = {}
with codecs.open(tz_path + "Docs/KALDI-CGN-IPA-WORD.txt", "r", "utf-8") as f:
    for c, i in enumerate(f, 0):
        if c > 0:
            l = i.split(",")
            if l[0] not in kaldi2ipa:
                kaldi2ipa[l[0]] = l[2]
            if l[1] not in cgn2ipa:
                cgn2ipa[l[1]] = l[2]

with codecs.open(tz_path + "clst-asr-fa/nnn_words.txt", "r", "utf-8") as f:
    nnn_words = [i[:-1] for i in f]

dst = panphon.distance.Distance()

with codecs.open(tens_path + "grid_search/" + gs_num + "_aligned_dist.csv", "w", "utf-8") as f:
    with codecs.open(tens_path + "grid_search/" + gs_num + "_aligned.csv", "r", "utf-8") as g:
        for l_num, l in enumerate(g, 1):
            print(l[:-1])
            line_l = l[:-1].split(",")
            if l_num == 1:
                ids = {i: n for n, i in enumerate(line_l, 0)}
                f.write(l[:-1] + ",cgn_ipa,kal_ipa,tran_dist,ends_in_en\n")
            else:
                cgn_tran = line_l[ids["cgn_tran"]]
                kal_tran = line_l[ids["kal_tran"]]
                word_ort = line_l[ids["word"]]
                word_ort = re.sub(r'[.?!]', "", word_ort.lower())
                word_ort = re.sub(r'\*.*', "", word_ort)
                if word_ort[-2:] in ["en", "ën"]:
                    ends_in_en = "FALSE"
                    for n in nnn_words:
                        if re.search(r'.*{}$'.format(n), word_ort):
                            ends_in_en = "TRUE"
                            break
                else:
                    ends_in_en = "FALSE"
                if not re.search(r"(SIL|SPN|\[\]|NA)", cgn_tran) and not re.search(r"(SIL|SPN|\[\]|NA)", kal_tran):
                    cgn_ipa = ""
                    for p in cgn_tran.split(" "):
                        cgn_ipa += cgn2ipa[p]
                    kal_ipa = ""
                    for p in kal_tran.split(" "):
                        kal_ipa += kaldi2ipa[p]
                    tran_dist = str(dst.weighted_feature_edit_distance(cgn_ipa, kal_ipa))
                else:
                    cgn_ipa = "NA"
                    kal_ipa = "NA"
                    tran_dist = "NA"
                new_line_l = line_l + [cgn_ipa, kal_ipa, tran_dist, ends_in_en]
                f.write(",".join(new_line_l) + "\n")
