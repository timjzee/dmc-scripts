import sys
import re
import math

tz_path = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"
tens_path = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
var_file = "p_f_type_O_merge_2syl_k4_ID_invar"

lexicon_dict = {}
lexicon_text = ""
with open(tz_path + "clst-asr-fa/lexicon_comp-acd-ifadv-ecsd-ko.txt", "r") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        lexicon_dict[entry] = pron
        lexicon_text += line

subtlex = {}
with open(tens_path + "other/SUBTLEX-NL.txt", "r") as f:
    for line in f:
        line_list = line[:-1].split("\t")
        word = line_list[0]
        subtlexwf = line_list[6]
        freqcount = line_list[1]
        subtlex[word] = [subtlexwf, freqcount]

neighbours_lex = {}
with open(tz_path + "Docs/neighbour_lexicon.txt", "r") as f:
    for counter, line in enumerate(f, 1):
        word, lev_neb, lev_neb_num, lev_neb_freq, nlev_neb, nlev_neb_num, nlev_neb_freq = line[:-1].split("\t")
        neighbours_lex[word] = [lev_neb_num, lev_neb_freq]

print("Loading COW word frequencies")
cow_uni = {}
with open(tens_path + "other/cow1.counts", "r") as f:
    for counter, line in enumerate(f, 1):
        wrd, freq = line[:-1].split("\t")
        cow_uni[wrd] = int(freq)
        if counter % 1000000 == 0:
            print(str((float(counter) / float(5052213)) * 100) + " %")

print("Loading COW bigram frequencies")
with open(tens_path + "other/cow2.counts", "r") as f:
    cow_bi = f.read()

with open(tens_path + "other/" + var_file + "2.csv", "w") as g:
    with open(tens_path + "timbl_files/" + var_file + ".csv", "r") as f:
        for n, l in enumerate(f, 1):
            l_list = l[:-1].split(",")
            if n == 1:
                new_line = ",".join(l_list + ["seg_info", "seg_info_token", "lex_neb", "phon_s_num", "phon_schwa_num", "phon_s_freq", "phon_schwa_freq", "informativity_prev", "informativity_next"]) + "\n"
            else:
                word = l_list[0]
                print(word)
                plural = word + "s"
                # Get Informativity
                if plural not in cow_uni:
                    informativity_prev = "NA"
                    informativity_next = "NA"
                else:
                    word_freq = cow_uni[plural]
                    word_freq += 1 if word_freq == 0 else 0
                    informativity_prev = 0
                    prev_bigrams = re.findall(r'[^\n]* {}\t[0-9]+'.format(plural), cow_bi)
                    for b in prev_bigrams:
                        prev_word = b.split(" ")[0]
                        b_freq = int(b.split("\t")[1])
                        b_freq += 1 if b_freq == 0 else 0
                        if prev_word not in cow_uni:
                            continue
                        else:
                            prev_word_freq = cow_uni[prev_word]
                            prev_word_freq += 1 if prev_word_freq == 0 else 0
                            predictability_prev = b_freq / word_freq
                            predictability_word = b_freq / prev_word_freq
                            informativity_prev += predictability_prev * math.log(predictability_word, 2)
                    informativity_prev = str(-1 * informativity_prev)
                    informativity_next = 0
                    next_bigrams = re.findall(r'^{} [^\n]+'.format(plural), cow_bi, flags=re.MULTILINE)
                    for b in next_bigrams:
                        next_word = b.split("\t")[0].split(" ")[1]
                        b_freq = int(b.split("\t")[1])
                        b_freq += 1 if b_freq == 0 else 0
                        if next_word not in cow_uni:
                            continue
                        else:
                            next_word_freq = cow_uni[next_word]
                            next_word_freq += 1 if next_word_freq == 0 else 0
                            predictability_next = b_freq / word_freq
                            predictability_word = b_freq / next_word_freq
                            informativity_next += predictability_next * math.log(predictability_word, 2)
                    informativity_next = str(-1 * informativity_next)
                # Get other variables
                f_s, f_en, f_oth = l_list[4:7]
                if word not in neighbours_lex:
                    lex_neb = "NA"
                else:
                    lex_neb = str(neighbours_lex[word][0])
                if word not in lexicon_dict:
                    seg_inf = "NA"
                    seg_inf_tok = "NA"
                else:
                    word_pron = lexicon_dict[word]
                    s_match_list = re.findall(r'[^\n]*\t{} s'.format(word_pron), lexicon_text)
                    s_match_n = len(s_match_list)
                    if word_pron[-1] == "@":
                        en_match_list = re.findall(r'[^\n]*\t{}'.format(word_pron), lexicon_text)
                    else:
                        en_match_list = re.findall(r'[^\n]*\t{} @'.format(word_pron), lexicon_text)
                    en_match_n = len(en_match_list)
                    all_match_list = re.findall(r'[^\n]*\t{}'.format(word_pron), lexicon_text)
                    all_match_n = len(all_match_list)
                    if s_match_n == 0 and f_s != "0":
                        s_match_n += 1
                        s_match_list.append("{}s\t{} s".format(word, word_pron))
                        all_match_n += 1
                        all_match_list.append("{}s\t{} s".format(word, word_pron))
                    if en_match_n == 0 and f_en != "0":
                        en_match_n += 1
                        if word_pron[-1] == "@":
                            en_match_list.append("{}n\t{}".format(word, word_pron))
                        else:
                            if word + "en" in subtlex:
                                en_match_list.append("{}en\t{}".format(word, word_pron))
                            elif word + "ën" in subtlex:
                                en_match_list.append("{}ën\t{}".format(word, word_pron))
                            elif word[:-1] + "ën" in subtlex:
                                en_match_list.append("{}ën\t{}".format(word[:-1], word_pron))
                            elif word + word[-1] + "en" in subtlex:
                                en_match_list.append("{}en\t{}".format(word + word[-1], word_pron))
                            elif word[:-2] + word[-1] + "en" in subtlex:        # ploegmaten
                                en_match_list.append("{}en\t{}".format(word[:-2] + word[-1], word_pron))
                            else:
                                en_match_list.append("{}en\t{}".format(word, word_pron))
                    if all_match_n == 0:
                        seg_inf = "NA"
                        seg_inf_tok = "NA"
                    else:
                        if s_match_n == 0:
                            seg_inf = "NA"
                        else:
                            seg_inf = str(-1 * math.log(s_match_n / all_match_n, 2))
                        s_match_freq = 0
                        en_match_freq = 0
                        all_match_freq = 0
                        for m in s_match_list:
                            mw = m.split("\t")[0]
                            s_match_freq += int(subtlex[mw][1]) if mw in subtlex else 0
                        for m in en_match_list:
                            mw = m.split("\t")[0]
                            en_match_freq += int(subtlex[mw][1]) if mw in subtlex else 0
                        for m in all_match_list:
                            mw = m.split("\t")[0]
                            all_match_freq += int(subtlex[mw][1]) if mw in subtlex else 0
                        if s_match_freq == 0 or all_match_freq == 0:
                            seg_inf_tok = "NA"
                        else:
                            seg_inf_tok = str(-1 * math.log(s_match_freq / all_match_freq, 2))
                print(word, seg_inf, seg_inf_tok, lex_neb, s_match_n, en_match_n, s_match_freq, en_match_freq, informativity_prev, informativity_next)
                new_line = ",".join(l_list + [seg_inf, seg_inf_tok, lex_neb, str(s_match_n), str(en_match_n), str(s_match_freq), str(en_match_freq), informativity_prev, informativity_next]) + "\n"
            g.write(new_line)
