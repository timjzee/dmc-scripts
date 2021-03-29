import sys
import json
import codecs

tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"

with codecs.open(tens_path + "other/dataset_invar2.json", "r") as f:
    dataset_invar = json.load(f)

with codecs.open(tens_path + "other/invar_distribution.csv", "w") as g:
    g.write("word,f_ev,f_s,f_en,f_other\n")
    for noun in dataset_invar:
        # exclude diminuatives
        if noun[-2:] != "je":
            f_s = dataset_invar[noun]["freq"] if dataset_invar[noun]["class"] == "S" else 0
            f_en = dataset_invar[noun]["freq"] if dataset_invar[noun]["class"] == "EN" else 0
            f_other = dataset_invar[noun]["freq"] if dataset_invar[noun]["class"] == "OTHER" else 0
            g.write("{},{},{},{},{}\n".format(noun, dataset_invar[noun]["ev"], f_s, f_en, f_other))
