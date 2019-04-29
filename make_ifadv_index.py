# -*- coding: utf-8 -*-

import re
import tempfile
import textgrid  # https://github.com/kylebgorman/textgrid
import sys
import glob
import codecs


tens_path = "/Volumes/tensusers/timzee/IFADVcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFADVcorpus/"

tg_folder = "Annotations/ort/"

encoding = "ascii"

repl_dict = {
    "trema": {"a": "ä", "e": "ë", "i": "ï", "o": "ö", "u": "ü", "y": "ÿ", "A": "Ä", "E": "Ë", "I": "Ï", "O": "Ö", "U": "Ü", "Y": "Ÿ"},
    "aigu": {"a": "á", "c": "ć", "e": "é", "i": "í", "n": "ń", "o": "ó", "s": "ś", "u": "ú", "y": "ý", "z": "ź", "A": "Á", "C": "Ć", "E": "É", "I": "Í", "N": "Ń", "O": "Ó", "S": "Ś", "U": "Ú", "Y": "Ý", "Z": "Ź"},
    "2aigu": {"o": "ő", "u": "ű", "O": "Ő", "U": "Ű"},
    "grave": {"a": "à", "e": "è", "i": "ì", "o": "ò", "u": "ù", "A": "À", "E": "È", "I": "Ì", "O": "Ò", "U": "Ù"},
    "circum": {"a": "â", "e": "ê", "i": "î", "o": "ô", "u": "û", "A": "Â", "E": "Ê", "I": "Î", "O": "Ô", "U": "Û"},
    "tilde": {"a": "ã", "n": "ñ", "o": "õ", "A": "Ã", "N": "Ñ", "O": "Õ"},
    "caron": {"c": "č", "d": "ď", "e": "ě", "g": "ǧ", "n": "ň", "r": "ř", "s": "š", "t": "ť", "z": "ž", "C": "Č", "D": "Ď", "E": "Ě", "G": "Ǧ", "N": "Ň", "R": "Ř", "S": "Š", "T": "Ť", "Z": "Ž"},
    "strike": {"d": "đ", "D": "Đ"},
    "ring": {"a": "å", "u": "ů", "A": "Å", "U": "Ů"},
    "ogonek": {"a": "ą", "e": "ę", "A": "Ą", "E": "Ę"},
    "ash": {"a": "æ", "A": "Æ"},
    "solidus": {"o": "ø", "l": "ł", "O": "Ø", "L": "Ł"},
    "cedille": {"c": "ç", "C": "Ç"},
    "sharp": {"s": "ß"},
    "thorn": {"t": "þ"},
    "dot": {"z": "ż", "Z": "Ż"}
}

filepaths = []
wavpaths = []
for fp in glob.glob(tens_path + tg_folder + "*.ort"):
    filepaths.append(fp)
    wp = re.search(r'DVA.*(?=_15min)', fp).group()
    wavpaths.append(wp)


def makeTempFile(fp):
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(open(fp).read())
    tempf.flush()
    return tempf


def replacePraatEscapes(i_ort):
    i_ort = re.sub(r'(\\[aeiouyAEIOUY]")', lambda m: repl_dict["trema"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aceinosuyzACEINOSUYZ]')", lambda m: repl_dict["aigu"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[ouOU]:)", lambda m: repl_dict["2aigu"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aeiouAEIOU]`)", lambda m: repl_dict["grave"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aeiouAEIOU]\^)", lambda m: repl_dict["circum"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[anoANO]~)", lambda m: repl_dict["tilde"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[cdegnrstzCDEGNRSTZ]<)", lambda m: repl_dict["caron"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[dD]-)", lambda m: repl_dict["strike"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aAuU]o)", lambda m: repl_dict["ring"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aAeE];)", lambda m: repl_dict["ogonek"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[aA]e)", lambda m: repl_dict["ash"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[oOlL]/)", lambda m: repl_dict["solidus"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[cC],)", lambda m: repl_dict["cedille"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\ss)", lambda m: repl_dict["sharp"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\th)", lambda m: repl_dict["thorn"][m.group()[1]], i_ort)
    i_ort = re.sub(r"(\\[zZ]!)", lambda m: repl_dict["dot"][m.group()[1]], i_ort)
    return i_ort


def read_file_entries(fp, fn):
    """read all 'interval' entries from a single file"""
    tg = textgrid.TextGrid()
    with makeTempFile(fp) as tempf:
        tg.read(tempf.name)  # read the textgrid
        # filter relevant tiers
#        tiers = filter(lambda tier: tier.name in ["spreker1", "spreker2"], tg)
        tier_num = 0
        for t_counter, tier in enumerate(tg, 1):
            # sanity check, so we assign correct chan and tier later on
            if tier.name in ["spreker1", "spreker2"]:
                tier_num += 1
                assert tier_num == int(tier.name[-1])
                for interval in tier:
                    i_start, i_end, i_ort = [interval.minTime, interval.maxTime, interval.mark]
                    i_ort = re.sub(r' (?=[ .,:;?!])', "", i_ort.strip(" "))
                    # special praat characters to unicode
                    i_ort = replacePraatEscapes(i_ort.encode("utf-8"))
                    i_ort = re.sub(r"[,\t\n\r]", "", i_ort)
                    if i_ort != "":
                        i_entry = [wavpaths[fn], str(tier_num), "{0:.3f}".format(i_start), "{0:.3f}".format(i_end), i_ort, str(t_counter)]
                        print(i_entry)
                        ifadv_index.append(",".join(i_entry))


textgrid.textgrid.detectEncoding = lambda f: encoding
ifadv_index = []
for counter, filepath in enumerate(filepaths, 0):
    print(filepath)
    read_file_entries(filepath, counter)

with codecs.open(tens_path + "ifadv_index.txt", "w", "utf-8") as f:
    for line in ifadv_index:
        f.write(line.decode("utf-8") + "\n")
