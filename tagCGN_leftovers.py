from frog import Frog, FrogOptions
import glob
import re

tens_path = "/vol/tensusers/timzee/cgn/"
tg_folder = "Annotations/ort/comp-d/"

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


with open(tens_path + "Annotations/missing_pos_files.txt", "r") as f:
    missing_files = [l[:-1] for l in f]

print("Loading .ort files")

filepaths = []
for fp in glob.glob(tens_path + tg_folder + "*.ort"):
    for mf in missing_files:
        if mf in fp:
            filepaths.append(fp)
            break

print("Loading speakers")

allowed_speakers = []
with open(tens_path + "speakers.txt", "r") as f:
    for num, line in enumerate(f, 1):
        if num > 1:
            allowed_speakers.append(line.split("\t")[4])

print("Extracting text")

txt_dict = {}

for fp in filepaths:
    file_n = fp.split("/")[-1].split(".")[0]
    txt_dict[file_n] = {}
    spkr = ""
    with open(fp, "r") as f:
        for line in f:
            if "name =" in line:
                spkr = re.search(r'(?<=name = ").*(?="[ ]*$)', line).group()
            if "text =" in line and spkr in allowed_speakers:
                line_txt = re.search(r'(?<=text = ").*(?="[ ]*$)', line).group()
#                print(line_txt)
                line_txt = replacePraatEscapes(line_txt)
                # remove codes, so they are not interpreted by Frog
                line_txt = re.sub(r"\*[^ \.!?,;:]*", "", line_txt)
                line_txt = re.sub(r'\\[vVoO]', '', line_txt)  # *u
                line_txt = re.sub(r'[\s]+', ' ', line_txt)
                line_txt = line_txt.strip(" ")
#                    line_txt = re.sub(r'\\\*$', '!', line_txt)  # prevent from being interpreted as SPEC(afk)
                line_txt = re.sub(r'\\\*', '', line_txt)         # *a
                line_txt = re.sub(r'\\-', ' ', line_txt)                    # spelling
                line_txt = re.sub(r'"', '', line_txt)
                line_txt = re.sub(r'[ ]+(?=[.,:;?!])', "", line_txt)
                line_txt = re.sub(r'(?<=[A-Za-z])\.\.\.(?=[A-Za-z])', ' ', line_txt)  # replace '...' with ' ' in first instance in 'heeft denk ik...wel...'
                line_txt = re.sub(r'[\.!]*[!]+[\.!]*', '!', line_txt)   # replace combos including at least 1 '!'
                line_txt = re.sub(r'[\.!?]*[?]+[\.!?]*', '?', line_txt)  # replace combos including at least 1 '?'
                line_txt = re.sub(r'\.+', '.', line_txt)   # replace clusters of '.' with a single '.'
#                line_txt = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', line_txt)  # gets rid of all unicode ?
                line_txt = re.sub(r" '(?=[a-z]+[ .!?,-])", " ''", line_txt)  # to make sure that preceding punctuation results in sentence demarcation
                line_txt = re.sub(r" '(?=[a-zA-Z-]+$)", " ''", line_txt)  # to make sure that preceding punctuation results in sentence demarcation
                line_txt = re.sub(r"^'(?=[a-z]+[ .!?,-])", "''", line_txt)  # such as 'kvind
                line_txt = re.sub(r"^'(?=[a-zA-Z-]+$)", "''", line_txt)  # such as 'kvind
                line_txt = re.sub(r"(?<=[ .!?,])da's(?=[ .!?,])", "da''s", line_txt)  # handle da's
                line_txt = re.sub(r"^da's(?=[ .!?,])", "da''s", line_txt)  # handle da's
                line_txt = re.sub(r"^da's$", "da''s", line_txt)  # handle da's
                line_txt = re.sub(r"(?<=[ .!?,])da's$", "da''s", line_txt)  # handle da's
                line_txt = re.sub(r"(?<=[a-z])'(?=[a-z][a-z])", "", line_txt)  # deal with d'rbij
                line_txt = re.sub(r'^\.', '', line_txt)
                line_txt = re.sub(r'[!?\.,:;]', lambda m: " " + m.group(), line_txt)  # prevent from being interpreted as SPEC(afk)
#                    if re.search(r'"".+""', line_txt):
#                        print(re.search(r'"".+""', line_txt).group())
#                if len(line_txt) > 0:
#                    if line_txt[-1] not in [".", ",", "!", "?", ":", ";"]:  # add . if chunk does not end in punctuation
#                            if re.search(r' [A-Za-z]$', line_txt):  # prevent from being interpreted as SPEC(afk)
#                                line_txt += "!"
#                            else:
#                        line_txt += " ."
                if spkr in txt_dict[file_n]:
                    txt_dict[file_n][spkr] += line_txt + " "
                else:
                    txt_dict[file_n][spkr] = line_txt + " "

print("Run Frog")

frog = Frog(FrogOptions(parser=True))


def tag_files(files):
    for fl in files:
        with open(tens_path + "Annotations/pos/comp-d/" + fl + ".pos", "w") as g:
            for spkr in txt_dict[fl]:
                print(fl, spkr)
                text = txt_dict[fl][spkr]
#                print(text)
                word_list = frog.process(text)
                print("BLA")
                s_counter = 0
                for word in word_list:
                    if word["index"] == "1":
                        s_counter += 1
                        g.write("< file id: {} speaker id: {} sentence: {} >\n".format(fl, spkr, s_counter))
                    if "_" in word["text"]:
                        for i_num, i in enumerate(word["text"].split("_"), 0):
                            g.write("\t".join([i, word["pos"].split("_")[i_num], word["lemma"].split("_")[i_num], str(word["posprob"]), word["dep"], str(word["depindex"]), str(word["index"]), "1"]) + "\n")
                    else:
                        g.write("\t".join([word["text"], word["pos"], word["lemma"], str(word["posprob"]), word["dep"], str(word["depindex"]), str(word["index"]), "0"]) + "\n")


tag_files(list(txt_dict.keys()))
