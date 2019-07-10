import sys
import codecs

if len(sys.argv) != 3:
    print("provide grep file and grepped file as arguments")
    sys.exit()
else:
    with codecs.open(sys.argv[1], "r", "utf-8") as f:
        tobecorrected = f.readlines()
    with codecs.open(sys.argv[2], "r", "utf-8") as g:
        for line in g:
            if line in tobecorrected:
                line_l = line.split(",")
                line_l[19] = "GEN-POSS"
                print(",".join(line_l)[:-1])
            else:
                print(line[:-1])
