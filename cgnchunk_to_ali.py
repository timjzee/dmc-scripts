#!/usr/bin/env python2

import sys
import textgrid
import decimal
import codecs

tz_path = "/Volumes/timzee/Docs/" if sys.platform == "darwin" else "/home/timzee/Docs/"
tens_path = "/Volumes/tensusers/timzee/cgn/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/"
cgn_path = "/Volumes/bigdata2/corpora2/CGN2/data/annot/" if sys.platform == "darwin" else "/vol/bigdata/corpora2/CGN2/data/annot/"


def createTemp(tg_path):
    with gzip.open(tg_path) as f:
        gz_bytes = f.read()
    tg_string = codecs.decode(gz_bytes, "iso-8859-1")
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(tg_string.encode("utf-8"))
    tempf.flush()
    return tempf


def loadTextGrid(tg_path):
    tg = textgrid.TextGrid()
    with createTemp(tg_path) as tempf:
        tg.read(tempf.name, encoding="utf-8")
    return tg


def readWriteAli():
    chunk_list = [line[:-1].split(",") for line in sys.stdin]
    for chunk in chunk_list:
        fp
        try:
            with open(fp, "r") as f:
                ali_lines = f.readlines()
        except ValueError:
            sys.exit()


if __name__ == '__main__':
    readWriteAli()
