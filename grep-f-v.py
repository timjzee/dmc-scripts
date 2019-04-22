import sys
import codecs

if len(sys.argv) != 3:
    print("provide grep file and grepped file as arguments")
    sys.exit()
else:
    with codecs.open(sys.argv[2], "r", "utf-8") as g:
        grepped_f = g.readlines()
    with codecs.open(sys.argv[1], "r", "utf-8") as f:
        for counter, grep_l in enumerate(f, 1):
#            if counter % 1000 == 0:
#                print(counter)
            for grepped_l in grepped_f:
                if grep_l[:-1] in grepped_l:
                    grepped_f.remove(grepped_l)
                    break
    for remaining in grepped_f:
        print(remaining[:-1])
