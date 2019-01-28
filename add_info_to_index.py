import sys
import re


tens_path = "/Volumes/tensusers/timzee/cgn/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/"
cgn_path = "/Volumes/bigdata2/corpora2/CGN2/data/annot/" if sys.platform == "darwin" else "/vol/bigdata/corpora2/CGN2/data/annot/"

with open(tens_path + "cgn_index_171218_pron.txt", "r") as f:
    for line in f:
        line_ls = re.sub(r'"', "", line).split(",")
