import sys
import codecs

if len(sys.argv) != 3:
    print("provide grep file and grepped file as arguments")
    sys.exit()
else:
    found_lines = []
    with codecs.open(sys.argv[2], "r", "utf-8") as g:
        grepped_f = g.readlines()
    with codecs.open(sys.argv[1], "r", "utf-8") as f:
        for counter, grep_l in enumerate(f, 1):
#            if counter % 1000 == 0:
#                print(counter)
            grep_list = grep_l.split(" ")
            id, subject = grep_list[:2]
            sentence = grep_list[9]
            for grepped_l in grepped_f:
                grepped_list = grepped_l.split(" ")
                if id == grepped_list[0] and subject == grepped_list[1] and sentence == grepped_list[10]:
                    found_lines.append(" ".join([id, subject, sentence, grepped_list[16]]))
#                    break
    for line in found_lines:
        print(line)
