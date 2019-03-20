import re
import tempfile
import textgrid  # https://github.com/kylebgorman/textgrid
import sys
import os
import glob
import codecs


tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"

tg_folder = "SLcorpus/Labels/chunks/"

encoding = "latin-1"

filepaths = []
wavpaths = []
for speaker in os.listdir(tens_path + tg_folder):
    if os.path.isdir(tens_path + tg_folder + speaker):
        for fp in glob.glob(tens_path + tg_folder + speaker + "/*.translit"):
            filepaths.append(fp)
            wp = re.search(r'[MF][0-9][0-9][A-Z]/.*(?=.translit)', fp).group()
            wavpaths.append(wp)


def makeTempFile(fp):
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(open(fp).read())
    tempf.flush()
    return tempf


def read_file_entries(fp, fn):
    """read all 'interval' entries from a single file"""
    tg = textgrid.TextGrid()
    with makeTempFile(fp) as tempf:
        tg.read(tempf.name)  # read the textgrid
        # filter relevant tiers
        tiers = filter(lambda tier: tier.name == "TRANSLIT", tg)
        for tier in tiers:
            i_counter = 0
            for interval in tier:
                i_start, i_end, i_ort = [interval.minTime, interval.maxTime, interval.mark]
                i_ort = re.search(r'^.*(?=__[FM][0-9][0-9])', i_ort).group().strip(" ") if re.search(r'^.*(?=__[FM][0-9][0-9])', i_ort) else i_ort.strip(" ")
                i_ort = re.sub(r' (?=[.,:;?!])', "", i_ort)
                i_ort = re.sub(r"(''|``)", "'", i_ort)
                i_ort = re.sub(r"`", "'", i_ort)
                i_ort = re.sub(r',', "", i_ort)
                i_ort = re.sub(r'[ ]?"', "", i_ort).strip(" ")
                if i_ort not in ["", "#"]:
                    i_counter += 1
                if i_counter > 1:
                    if ifa_index[-1][-2][-1] in ["!", "?", "."]:    # handle uppercase due to sentence start
                        i_ort = re.sub(r"^'s[ -][A-Z][a-z]", lambda x: x.group(0).lower(), i_ort)
#                        i_ort = re.sub(r'^[A-Z][a-z ]', lambda x: x.group(0).lower(), i_ort)
                i_ort = re.sub(r"'s (?=morgens|middags|avonds|ochtends|nachts)", "'s-", i_ort)    # necessary because IFA POS annotation views these as 1 word
                if i_ort not in ["", "#"]:
                    i_entry = [wavpaths[counter], "2", "{0:.3f}".format(i_start), "{0:.3f}".format(i_end), i_ort, "1"]
                    print(i_entry)
                    ifa_index.append(i_entry)


textgrid.textgrid.detectEncoding = lambda f: encoding
ifa_index = []
for counter, filepath in enumerate(filepaths, 0):
    print(filepath)
    read_file_entries(filepath, counter)

with codecs.open(tens_path + "ifa_index.txt", "w", "utf-8") as f:
    for line in ifa_index:
        f.write(",".join(line) + "\n")
