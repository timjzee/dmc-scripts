# -*- coding: utf-8 -*-

import sys
import codecs
import re
import subprocess
from telwoord import cardinal

home_dir = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"
tens_dir = "/Volumes/tensusers/timzee/" if sys.platform == "darwin" else "/vol/tensusers/timzee/"
corpus = "IFADVcorpus"

print("Loading KALDI lexicon")
kaldi_lex = {}
with codecs.open(home_dir + "clst-asr-fa/lexicon_from_MARIO.txt", "r", "utf-8") as f:
    for line in f:
        entry, pron = line[:-1].split("\t")
        if entry in kaldi_lex:
            pass
        else:
            kaldi_lex[entry] = pron

with codecs.open(tens_dir + corpus + "/ifadv_index.txt", "r", "utf-8") as f:
    cgn_index = f.readlines()

if len(sys.argv) > 1:
    core_num = int(sys.argv[1])
    from_l = int(sys.argv[2])
    to_l = int(sys.argv[3])
else:
    core_num = 0
    from_l = 1
    to_l = len(cgn_index) + 1

interjections = {"eih": "EI", "eikes": "EI k @ s", "mm-hu": "m h U", "oesje": "u S @", "oho": "o h o", "sjt": "S t", "uhu": "@ h @", "woh": "w o", "zuh": "z U", "zulle": "z U l @"}


def g2p(wrd):
    """Calls Kaldi phonetisaurus"""
    print("Working on G2P: " + wrd)
    with codecs.open(home_dir + "fa_files/oov{}.tmp".format(core_num), "w", "utf-8") as f:
        f.write(wrd)
    output = subprocess.check_output([home_dir + "fa_files/run_phonetisaurus.sh", home_dir + "fa_files/oov{}.tmp".format(core_num)]).decode("utf-8")
    return output[:-1].split("\t")


def decompound(cmpnd):
    """Calls Louis' decompounding script"""
    print("Working on decompounding: " + cmpnd)
    output = subprocess.check_output([home_dir + "fa_files/run_decompounding.sh", cmpnd]).decode("utf-8")[:-1].split(" ")
    compound_lex[cmpnd] = output
    return output


oov_lex = {}
conversion_table = {}
cgn_codes = {"*v": "foreign_word", "*d": "dialect_word", "*z": "accented", "*n": "neologism", "*t": "interjection", "*a": "incomplete", "*u": "mispronunciation", "*x": "unclear"}
compound_lex = {}

f = codecs.open(tens_dir + corpus + "/prepared_index{}.txt".format(core_num), "w", "utf-8")
# g = codecs.open(tens_dir + corpus + "/prep_log.txt", "w", "utf-8")

for counter, l in enumerate(cgn_index[from_l - 1:to_l], 1):
    wav, chan, from_t, to_t, ort, tier = l[:-1].split(",")
    print(core_num, l[:-1])
    if chan == "chan":
        continue
#    if "xxx" in ort or "Xxx" in ort:
#        g.write("line {}: 'xxx' in ort\n".format(counter))
#        continue
    words = [w for w in re.sub(r'[.!?\n]+', '', ort).split(" ") if w not in ["", " "]]
    new_words = []
    sw_i = 0
    for w_counter, w in enumerate(words, 1):
        sw_i += 1
        meta = (w_counter, [])
        sw_l = []
        si_l = []
        if "*" in w:
            for code in cgn_codes:
                if code in w:
                    meta[1].append(cgn_codes[code])
                    break   # CGN only allowed 1 code per word
        sws = [sw for sw in re.split(r"([-&])", w) if re.sub(r'\*[a-z]', '', sw) not in ["", "-"]]
        if len(sws) > 1:
            meta[1].append("hyphen/ampersand")
            pass
#            g.write("line {} word {}: '-' in word {}\n".format(counter, w_counter, w))
        for sw_counter, sw in enumerate(sws, 1):
            sw_i += 1 if sw_counter > 1 else 0
            sw_clean = re.sub(r'\*[a-z]', '', sw)
            if sw_clean.lower() in kaldi_lex:
                new_words.append(sw_clean.lower())
                sw_l.append(sw_clean.lower())
                si_l.append(str(sw_i))
            else:     # check for '
                sw_match1 = re.match(r"[qwrtpsdfghjklzxcvbnm]?'[qwrtpsdfghjklzxcvbnm]", sw_clean)
                sw_match2 = re.search(r"(?<=[\w])'[a-z]+\*?[a-z]?", sw)
                if sw_match1:   # handle 'tzelfde, d'rin
                    sw1 = sw_match1.group()
                    sw2 = sw[sw_match1.span()[1]:]
                    apo_sw = [sw for sw in [sw1, sw2] if re.sub(r'\*[a-z]', '', sw) != ""]
                elif sw_match2:  # handle MP3'tje, papa's, HBO'er
                    sw2 = sw_match2.group()
                    sw1 = sw[:sw_match2.span()[0]]
                    apo_sw = [sw for sw in [sw1, sw2] if re.sub(r'\*[a-z]', '', sw) != ""]
                else:   # if no ' in sw or in case somethign goes wrong
                    apo_sw = [ssw for ssw in sw.split("'") if re.sub(r'\*[a-z]', '', ssw) != ""]
                if len(apo_sw) > 1:
                    meta[1].append("apostrophe")
#                    g.write("line {} word {}: ' in sub-word {}\n".format(counter, w_counter, sw))
                for ssw_counter, ssw in enumerate(apo_sw, 1):
                    sw_i += 1 if ssw_counter > 1 else 0
                    ssw_clean = re.sub(r'\*[a-z]', '', ssw)
                    if ssw_clean.lower() in kaldi_lex:
                        new_words.append(ssw_clean.lower())
                        sw_l.append(ssw_clean.lower())
                        si_l.append(str(sw_i))
                    else:   # handle Quadro2, MP3
                        ssw_parts = [p for p in re.split(r'([0-9]+(?:\*[a-z])?)', ssw) if p != ""]
                        if len(ssw_parts) > 1:
                            meta[1].append("numeral")
#                            g.write("line {} word {}: numeral in sub-word {}\n".format(counter, w_counter, ssw))
                        for sssw_counter, sssw in enumerate(ssw_parts, 1):
                            sw_i += 1 if sssw_counter > 1 else 0
                            sssw_clean = re.sub(r'\*[a-z]', '', sssw)
                            if sssw_clean.lower() in kaldi_lex:
                                new_words.append(sssw_clean.lower())
                                sw_l.append(sssw_clean.lower())
                                si_l.append(str(sw_i))
                            else:   # handle initialisms, e.g. ALV
                                sssw_match = re.search(r"[A-Z][A-Z]+(?:\*[a-z])?", sssw)
                                if sssw_match:
                                    upper_list = re.findall(r'[A-Z](?:\*[a-z])?', sssw_match.group())
                                    before = sssw[:sssw_match.span()[0]]
                                    after = sssw[sssw_match.span()[1]:]
                                    sssw_parts = [p for p in [before] + upper_list + [after] if p != ""]
                                else:
                                    sssw_parts = [sssw]
                                if len(sssw_parts) > 1:
                                    meta[1].append("initialism")
#                                    g.write("line {} word {}: initialism in sub-word {}\n".format(counter, w_counter, sssw))
                                for ssssw_counter, ssssw in enumerate(sssw_parts, 1):
                                    sw_i += 1 if ssssw_counter > 1 else 0
                                    ssssw_clean = re.sub(r'\*[a-z]', '', ssssw)
                                    if ssssw_clean.lower() in kaldi_lex:
                                        new_words.append(ssssw_clean.lower())
                                        sw_l.append(ssssw_clean.lower())
                                        si_l.append(str(sw_i))
                                    else:   # now we'll test for all possible OOVs
                                        if re.fullmatch(r'[0-9]+', ssssw_clean):    # if ssssw is a number
                                            if "numeral" not in meta[1]:
                                                meta[1].append("numeral")
                                            num_list = [p for p in re.split(r'(^0+(?=[1-9][0-9]*))', ssssw_clean) if p != ""]
                                            if len(num_list) > 1:   # if there are leading zeroes
                                                assert len(num_list) == 2
                                                for zero in num_list[0]:
                                                    new_words.append("nul")
                                                    sw_l.append("nul")
                                                    si_l.append(str(sw_i))
                                                    sw_i += 1
                                                number = cardinal(int(num_list[1]), friendly=False)
                                            else:   # if there are no leading zeroes
                                                number = cardinal(int(ssssw_clean), friendly=False)
                                            number = re.sub(r'een', 'één', number)
                                            numbers = number.split(" ")  # sometimes cardinal() splits up numbers: 1001 --> duizend een
                                            for n_counter, n in enumerate(numbers, 1):
                                                sw_i += 1 if n_counter > 1 else 0
                                                if n in kaldi_lex:
                                                    new_words.append(n)
                                                    sw_l.append(n)
                                                    si_l.append(str(sw_i))
                                                else:   # handle OOV numbers like drieënnegentig
                                                    if (n not in kaldi_lex) and (n not in oov_lex):
                                                        oov_entry, oov_tran = g2p(n)
                                                        oov_lex[oov_entry] = oov_tran
                                                    new_words.append(n)
                                                    sw_l.append(n)
                                                    si_l.append(str(sw_i))
                                                    # comment out above code and uncomment below if you want decompounding instead of G2P
#                                                    n_list = compound_lex[n] if n in compound_lex else decompound(n)
#                                                    for c_counter, c in enumerate(n_list, 1):
#                                                        sw_i += 1 if c_counter > 1 else 0
#                                                        if (c not in kaldi_lex) and (c not in oov_lex):
#                                                            oov_entry, oov_tran = g2p(c)
#                                                            oov_lex[oov_entry] = oov_tran
#                                                        new_words.append(c)
#                                                        sw_l.append(c)
#                                                        si_l.append(str(sw_i))
                                        elif ssssw_clean.lower() == "ggg":  # if ssssw is speaker noise
                                            meta[1].append("speaker_noise")
#                                            g.write("line {} word {}: 'ggg' in (sub-)word {}\n".format(counter, w_counter, ssssw))
                                            if "spn" not in oov_lex:
                                                oov_entry, oov_tran = ["spn", "[SPN]"]
                                                oov_lex[oov_entry] = oov_tran
                                            # using BNF
                                            new_words.append("( ha | haha | hahaha | hahahaha | hahahahaha | spn )")
                                            sw_l.append("ggg")
                                            si_l.append(str(sw_i))
                                        elif ssssw_clean.lower() == "xxx":  # if ssssw is unintelligible
                                            meta[1].append("unintelligible")
#                                            g.write("line {} word {}: 'xxx' in (sub-)word {}\n".format(counter, w_counter, ssssw))
                                            if "spn" not in oov_lex:
                                                oov_entry, oov_tran = ["spn", "[SPN]"]
                                                oov_lex[oov_entry] = oov_tran
                                            # using BNF
                                            new_words.append("( ha | haha | hahaha | hahahaha | hahahahaha | spn )")
                                            sw_l.append("xxx")
                                            si_l.append(str(sw_i))
                                        elif ssssw_clean.lower() == "&":
                                            new_words.append("en")
                                            sw_l.append("en")
                                            si_l.append(str(sw_i))
                                        elif re.fullmatch(r'[A-Z][^A-Z0-9]+', ssssw_clean):    # if ssssw is a name
                                            meta[1].append("name")
#                                            g.write("line {} word {}: name in (sub-)word {}\n".format(counter, w_counter, ssssw))
                                            if ssssw_clean.lower() not in oov_lex:
                                                oov_entry, oov_tran = g2p(ssssw_clean.lower())
                                                oov_lex[oov_entry] = oov_tran
                                            new_words.append(ssssw_clean.lower())
                                            sw_l.append(ssssw_clean.lower())
                                            si_l.append(str(sw_i))
                                        elif ssssw_clean.lower() in interjections:  # if ssssw is an interjection
                                            meta[1].append("interjection")
#                                            g.write("line {} word {}: interjection in (sub-)word {}\n".format(counter, w_counter, ssssw))
                                            if ssssw_clean.lower() not in oov_lex:
                                                oov_lex[ssssw_clean.lower()] = interjections[ssssw_clean.lower()]
                                            new_words.append(ssssw_clean.lower())
                                            sw_l.append(ssssw_clean.lower())
                                            si_l.append(str(sw_i))
                                        elif "'" in ssssw_clean:    # if ssssw is OOV suffix with apostrophe
                                            stripped = re.sub(r"'", '', ssssw_clean.lower())
                                            if (stripped not in kaldi_lex) and (stripped not in oov_lex):
                                                oov_entry, oov_tran = g2p(stripped)
                                                oov_lex[oov_entry] = oov_tran
                                            new_words.append(stripped)
                                            sw_l.append(stripped)
                                            si_l.append(str(sw_i))
                                        elif "*" in ssssw:  # if OOV has a CGN code
                                            if ssssw_clean.lower() not in oov_lex:
                                                oov_entry, oov_tran = g2p(ssssw_clean.lower())
                                                oov_lex[oov_entry] = oov_tran
                                            new_words.append(ssssw_clean.lower())
                                            sw_l.append(ssssw_clean.lower())
                                            si_l.append(str(sw_i))
#                                            g.write("line {} word {}: CGN code in (sub-)word {}\n".format(counter, w_counter, ssssw))
                                        else:   # if OOV is a compound or part of an already split-up word
                                            if len(sws) == 1 and len(apo_sw) == 1 and len(ssw_parts) == 1 and len(sssw_parts) == 1:     # if OOV is a compound
                                                meta[1].append("compound")
#                                                g.write("line {} word {}: compound in (sub-)word {}\n".format(counter, w_counter, ssssw))
                                                if (ssssw_clean.lower() not in kaldi_lex) and (ssssw_clean.lower() not in oov_lex):
                                                    oov_entry, oov_tran = g2p(ssssw_clean.lower())
                                                    oov_lex[oov_entry] = oov_tran
                                                new_words.append(ssssw_clean.lower())
                                                sw_l.append(ssssw_clean.lower())
                                                si_l.append(str(sw_i))
                                                # comment out above code and uncomment below if you want decompounding instead of G2P
#                                                ssssw_list = compound_lex[ssssw_clean.lower()] if ssssw_clean.lower() in compound_lex else decompound(ssssw_clean.lower())
#                                                for i_counter, i in enumerate(ssssw_list, 1):
#                                                    sw_i += 1 if i_counter > 1 else 0
#                                                    if (i not in kaldi_lex) and (i not in oov_lex):
#                                                        oov_entry, oov_tran = g2p(i)
#                                                        oov_lex[oov_entry] = oov_tran
#                                                    new_words.append(i)
#                                                    sw_l.append(i)
#                                                    si_l.append(str(sw_i))
                                            else:   # if OOV is part of an already split-up word
                                                meta[1].append("other")
#                                                g.write("line {} word {}: leftover in (sub-)word {}\n".format(counter, w_counter, ssssw))
                                                if (ssssw_clean.lower() not in kaldi_lex) and (ssssw_clean.lower() not in oov_lex):
                                                    oov_entry, oov_tran = g2p(ssssw_clean.lower())
                                                    oov_lex[oov_entry] = oov_tran
                                                new_words.append(ssssw_clean.lower())
                                                sw_l.append(ssssw_clean.lower())
                                                si_l.append(str(sw_i))
        if len(sw_l) > 1 or len(meta[1]) > 0:
            chunk_id = ",".join([wav, tier, from_t, to_t])
            if chunk_id not in conversion_table:
                conversion_table[chunk_id] = {"input_words": [], "original_words": [], "input_i": [], "original_i": [], "meta_indices": [], "meta_info": []}
            if len(sw_l) > 1:
                conversion_table[chunk_id]["input_words"].append(" ".join(sw_l))
                conversion_table[chunk_id]["original_words"].append(w)
                conversion_table[chunk_id]["input_i"].append(" ".join(si_l))
                conversion_table[chunk_id]["original_i"].append(str(w_counter))
            if len(meta[1]) > 0:
                conversion_table[chunk_id]["meta_indices"].append(str(meta[0]))
                conversion_table[chunk_id]["meta_info"].append(";".join(meta[1]))
    f.write(",".join([wav, chan, from_t, to_t] + [" ".join(new_words)] + [tier]) + "\n")
f.close()
# g.close()

with codecs.open(home_dir + "clst-asr-fa/oov_lex{}.txt".format(core_num), "w", "utf-8") as f:
    for key in oov_lex:
        f.write(key + "\t" + oov_lex[key] + "\n")

with codecs.open(tens_dir + corpus + "/oov_conv_table{}.txt".format(core_num), "w", "utf-8") as f:
    f.write("chunk_id\tinput_words\toriginal_words\tinput_i\toriginal_i\tmeta_indices\tmeta_info\n")
    for key in conversion_table:
        f.write("\t".join([key, ",".join(conversion_table[key]["input_words"]), ",".join(conversion_table[key]["original_words"]), ",".join(conversion_table[key]["input_i"]), ",".join(conversion_table[key]["original_i"]), ",".join(conversion_table[key]["meta_indices"]), ",".join(conversion_table[key]["meta_info"])]) + "\n")

print("Finished processing OOVs")
# if len(sys.argv) == 1:
#    subprocess.call([home_dir + "fa_files/run_lexical_expansion.sh"])
