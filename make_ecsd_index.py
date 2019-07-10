# -*- coding: utf-8 -*-

import re
import tempfile
import textgrid  # https://github.com/kylebgorman/textgrid
import sys
import glob
import codecs


tens_path = "/Volumes/tensusers/timzee/ECSD/" if sys.platform == "darwin" else "/vol/tensusers/timzee/ECSD/"

tg_folder = "Annotations/ort/"

encoding = "utf-8"

filepaths = []
wavpaths = []
for fp in glob.glob(tens_path + tg_folder + "*/*.TextGrid"):
    filepaths.append(fp)
    wp = re.search(r'pp.*(?=\.TextGrid)', fp).group()
    wavpaths.append(wp)


def makeTempFile(fp):
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(open(fp).read())
    tempf.flush()
    return tempf


def read_file_entries(fp):
    """read all 'interval' entries from a single file"""
    file_n = wavpaths[counter]
    pair_n, part_n = file_n.split("_part")
    file_n_lc = pair_n.upper() + "_part" + part_n
    sp1_n, sp2_n = pair_n[2:].split("_")
    tg = textgrid.TextGrid()
    with makeTempFile(fp) as tempf:
        tg.read(tempf.name)  # read the textgrid
        # filter relevant tiers
#        tiers = filter(lambda tier: tier.name in ["spreker1", "spreker2"], tg)
        tier_num = 0
        for t_counter, tier in enumerate(tg, 1):
            # sanity check, so we assign correct chan and tier later on
            if tier.name in [sp1_n, sp2_n]:
                tier_num += 1
                assert tier_num == t_counter
                for interval in tier:
                    i_start, i_end, i_ort = [interval.minTime, interval.maxTime, interval.mark]
                    i_ort = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', i_ort.strip('" \t\n\r'))
                    i_ort = re.sub(r'[ ]+(?=[.,:;?!])', "", i_ort)
                    i_ort = re.sub(r'[!?\.,:;](?=[A-Za-z])', lambda m: m.group() + " ", i_ort)
                    i_ort = re.sub(r'[.?!]+', '.', i_ort)
                    i_ort = re.sub(r'(?<=[A-Za-z])&(?=[A-Za-z])', ' & ', i_ort)
                    words = []
                    for w in i_ort.split(" "):
                        w = re.sub(r'\s', '', w)
                        w = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', w)
                        if w not in ["", " "]:
                            if "." in w:
                                for c, sw in enumerate(w.split("."), 1):
                                    if sw not in ["", '"']:
                                        if c != len(w.split(".")):
                                            words.append(sw + ".")
                                        else:
                                            words.append(sw)
                            else:
                                words.append(w)
                    final_words = []
                    for w_n, w in enumerate(words, 1):
                        w_ort = re.sub(r'[\\/][vVoO]', '*u', w)  # *u
                        w_ort = re.sub(r'[\\/]\*', '*a', w_ort)         # *a
                        w_ort = re.sub(r'[\\/]-', '', w_ort)                    # spelling
                        w_ort = re.sub(r'[",:;?!\n\r\t]', "", w_ort)
                        if w_ort not in ["", " "]:
                            final_words.append(w_ort)
                    final_ort = " ".join(final_words)
                    if final_ort != "":
                        i_entry = [file_n, str(tier_num), "{0:.3f}".format(i_start), "{0:.3f}".format(i_end), final_ort, str(t_counter)]
                        print(i_entry)
                        ecsd_index.append(",".join(i_entry))


textgrid.textgrid.detectEncoding = lambda f: encoding
ecsd_index = []
for counter, filepath in enumerate(filepaths, 0):
    print(filepath)
    read_file_entries(filepath)

with codecs.open(tens_path + "ecsd_index.txt", "w", "utf-8") as f:
    for line in ecsd_index:
        f.write(line + "\n")
