import sys
import os
import glob
import scipy.io.wavfile
import python_speech_features
import tempfile
import textgrid
import numpy as np
import multiprocessing
import re

# tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"
af_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"


chunk_folder = "training_chunks_en/o/"
# tg_folder = "SLcorpus/Labels/sentences/"

encoding = "latin-1"

corpora = {
    "cgn-a": np.array([1., 0., 0., 0., 0., 0., 0., 0.]),
    "cgn-c": np.array([0., 1., 0., 0., 0., 0., 0., 0.]),
    "cgn-d": np.array([0., 0., 1., 0., 0., 0., 0., 0.]),
    "cgn-k": np.array([0., 0., 0., 1., 0., 0., 0., 0.]),
    "cgn-o": np.array([0., 0., 0., 0., 1., 0., 0., 0.]),
    "ifadv": np.array([0., 0., 0., 0., 0., 1., 0., 0.]),
    "ecsd": np.array([0., 0., 0., 0., 0., 0., 1., 0.]),
    "ifa": np.array([0., 0., 0., 0., 0., 0., 0., 1.])
}

features = ["@", "n", "~"]

vowels = ["@", "A", "AU", "E", "E2", "EI", "EU", "I", "O", "U", "UI", "a", "e", "i", "o", "u", "y"]
vowels2 = vowels + [v + "#" for v in vowels]

cgn_codes = ["foreign_word", "dialect_word", "accented", "neologism", "interjection", "incomplete", "mispronunciation", "unclear"]

window = 0.025
step = 0.005
# prop_used = 0.4     # proportion of each interval that is used

# number of preceding and subsequent frames used for training sample
# this creates a buffer at the start and end of the recording for which we have no training sample
# if frame_window = 5, and step = .005, we have a buffer of .025 ms
# we need to take this into account when we predict new data and construct the tg
frame_window = 5


def makeTempFile(fp):
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(open(fp, "rb").read())
    tempf.flush()
    return tempf


def getFeatureLabel(frame_start, frame_end, feature, seg_index, segments, words, word_syllables, word_segments, comment_intervals):
    int_dur = round(segments.intervals[seg_index].duration(), 3)
    seg_time = segments.intervals[seg_index].minTime
    word_i = words.indexContaining(seg_time + 0.001)
    word = words.intervals[word_i]
    phon = segments.intervals[seg_index].mark
    next_phon = segments.intervals[seg_index + 1].mark if seg_index < len(segments.intervals) - 1 else None
    next_phon_str = next_phon.strip("[]#") if next_phon else None
    prev_phon = segments.intervals[seg_index - 1].mark if seg_index > 0 else None
    prev_phon_str = prev_phon.strip("[]#") if prev_phon else None
    next_next_phon = segments.intervals[seg_index + 2].mark if seg_index + 1 < len(segments.intervals) - 1 else None
    next_next_phon_str = next_next_phon.strip("[]#") if next_next_phon else None
    prev_prev_phon = segments.intervals[seg_index - 2].mark if seg_index - 1 > 0 else None
    prev_prev_phon_str = prev_prev_phon.strip("[]#") if prev_prev_phon else None
    word_str = word.mark.strip("?!.,")
    final_n = re.search(r'([^eo]e|ë)n$', word_str)
    # general exclusion modes
    meta = comment_intervals[word_i].mark
    if re.search("(" + "|".join(cgn_codes) + ")", meta):
        return -1
    if phon.strip("[]# ") == "SPN" or prev_phon_str == "SPN" or next_phon_str == "SPN":
        return -1
    if word.mark.strip("?!.,") in ["uh", "uhm", "het"]:  # extend this mm-hu
        return -1
    if seg_index == 0:
        return -1
    # feature-specific criteria; see /notes/2021/17jun.txt /notes/2021/29jun.txt /notes/2021/5jul.txt
    if feature == "@":
        # check to see if phone is in word-final syllable
        num_rem_syls = 0
        for rem_int in segments.intervals[seg_index:]:
            if rem_int.mark in vowels2:
                num_rem_syls += 1
            if rem_int.mark[-1] == "#":
                break
        # positive
        if phon.strip("#") == "@" and num_rem_syls <= 1 and final_n:
            return -1
        # see 2aug.txt for reversed criteria
#        elif phon.strip("#") == "@" and prev_phon_str in ["p", "t", "k", "f", "s", "x", "v", "z", "G"] and next_phon_str in ["r", "l"]:
#            return -1
        elif phon.strip("#") == "@" and prev_phon_str in ["b", "d", "g"] and next_phon_str in ["b", "d", "g", "l"]:
            return -1
        elif phon.strip("#") in vowels and prev_phon_str in ["", "SIL"]:
            return -1
        elif phon.strip("#") in vowels:
            # if next_phon_str in ["n", "m", "N"] and phon[-1] != "#":
            #     prop_used = 0.7
            #     prop_dur = int_dur * prop_used
            #     used_e = round(segments.intervals[seg_index].maxTime, 3)
            #     used_s = used_e - prop_dur
            # else:
            prop_used = 0.4
            prop_dur = int_dur * prop_used
            used_s = round(segments.intervals[seg_index].minTime, 3) + (int_dur - prop_dur) / 2
            used_e = used_s + prop_dur
            x1 = used_s - frame_start
            x1 = 0 if x1 <= 0 else window if x1 > window else x1
            x2 = frame_end - used_e
            x2 = 0 if x2 <= 0 else window if x2 > window else x2
            prop_f_in_used_i = (window - (x1 + x2)) / window
            if prop_f_in_used_i <= 0.5:
                return -1
            else:
                return 1
        # negative
        elif phon.strip("[]#") not in vowels + ["w", "j", "r"]:
            if phon.strip("#") in ["n", "N", "m"] and phon[-1] == "#" and final_n:  # (final_n or word.mark.strip("?!.,") == "een"):
                return -1
            else:
                if (prev_phon_str not in vowels + ["w", "j", "r"] or (prev_phon_str in vowels and phon.strip("#") in ["n", "N", "m"])) and (prev_phon[-1] != "#" or phon.strip("[]#") in ["SIL", ""]) and (next_phon_str not in vowels + ["w", "j", "r"] or (phon.strip("[]#") in ["p", "t", "k", "f", "s", "x", "v", "z", "G"] and next_phon_str == "@")) and (phon[-1] != "#" or next_phon_str in ["SIL", ""]):
                    prop_used = 1
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3)
                    used_e = used_s + prop_dur
                elif (prev_phon_str not in vowels + ["w", "j", "r"] or (prev_phon_str in vowels and phon.strip("#") in ["n", "N", "m"])) and (prev_phon[-1] != "#" or phon.strip("[]#") in ["SIL", ""]):
                    prop_used = 0.7
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3)
                    used_e = used_s + prop_dur
                elif (next_phon_str not in vowels + ["w", "j", "r"] or (phon.strip("[]#") in ["p", "t", "k", "f", "s", "x", "v", "z", "G"] and next_phon_str == "@")) and (phon[-1] != "#" or next_phon_str in ["SIL", ""]):
                    prop_used = 0.7
                    prop_dur = int_dur * prop_used
                    used_e = round(segments.intervals[seg_index].maxTime, 3)
                    used_s = used_e - prop_dur
                else:
                    prop_used = 0.4
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3) + (int_dur - prop_dur) / 2
                    used_e = used_s + prop_dur
                x1 = used_s - frame_start
                x1 = 0 if x1 <= 0 else window if x1 > window else x1
                x2 = frame_end - used_e
                x2 = 0 if x2 <= 0 else window if x2 > window else x2
                prop_f_in_used_i = (window - (x1 + x2)) / window
                if prop_f_in_used_i <= 0.5:
                    return -1
                else:
                    return 0
        # exclude
        else:
            return -1
    elif feature == "n":
        # check to see if phone is in word-final syllable
        num_rem_syls = 0
        for rem_int in segments.intervals[seg_index:]:
            if rem_int.mark in vowels2:
                num_rem_syls += 1
            if rem_int.mark[-1] == "#":
                break
        # positive
        if phon.strip("#") in ["n", "m", "N"] and final_n and phon[-1] == "#":
            return -1
        elif phon.strip("#") in ["n", "m", "N"]:
            if prev_phon in ["n", "m", "N"]:
                prop_used = 0.7
                prop_dur = int_dur * prop_used
                used_s = round(segments.intervals[seg_index].minTime, 3)
                used_e = used_s + prop_dur
            elif next_phon_str in ["n", "m", "N"] and num_rem_syls >= 1:  # exclude komen -> komn
                prop_used = 0.7
                prop_dur = int_dur * prop_used
                used_e = round(segments.intervals[seg_index].maxTime, 3)
                used_s = used_e - prop_dur
            else:
                prop_used = 0.4
                prop_dur = int_dur * prop_used
                used_s = round(segments.intervals[seg_index].minTime, 3) + (int_dur - prop_dur) / 2
                used_e = used_s + prop_dur
            x1 = used_s - frame_start
            x1 = 0 if x1 <= 0 else window if x1 > window else x1
            x2 = frame_end - used_e
            x2 = 0 if x2 <= 0 else window if x2 > window else x2
            prop_f_in_used_i = (window - (x1 + x2)) / window
            if prop_f_in_used_i <= 0.5:
                return -1
            else:
                return 1
        # exclude
        elif phon.strip("#") in ["t", "d", "j"] and prev_phon_str == "n":
            return -1
        elif phon.strip("#") in ["p", "b"] and prev_phon_str == "m":
            return -1
        elif phon.strip("#") in ["k", "g"] and prev_phon_str == "N":
            return -1
        # negative
        elif phon.strip("#") == "@" and num_rem_syls <= 1 and final_n:
            return -1
        else:
            if prev_phon_str not in ["n", "m", "N"] and prev_phon[-1] != "#" and not (prev_phon_str in ["t", "d", "j"] and prev_prev_phon_str == "n") and not (prev_phon_str in ["p", "b"] and prev_prev_phon_str == "m") and not (prev_phon_str in ["k", "g"] and prev_prev_phon_str == "N") and next_phon_str not in ["n", "m", "N"] and phon[-1] != "#" and not (phon.strip("#") in vowels and next_phon_str == "@" and num_rem_syls <= 2 and final_n) and not (phon.strip("#") not in vowels and next_phon_str == "@" and num_rem_syls <= 1 and final_n):
                prop_used = 1
                prop_dur = int_dur * prop_used
                used_s = round(segments.intervals[seg_index].minTime, 3)
                used_e = used_s + prop_dur
            elif prev_phon_str not in ["n", "m", "N"] and prev_phon[-1] != "#" and not (prev_phon_str in ["t", "d", "j"] and prev_prev_phon_str == "n") and not (prev_phon_str in ["p", "b"] and prev_prev_phon_str == "m") and not (prev_phon_str in ["k", "g"] and prev_prev_phon_str == "N"):
                prop_used = 0.7
                prop_dur = int_dur * prop_used
                used_s = round(segments.intervals[seg_index].minTime, 3)
                used_e = used_s + prop_dur
            elif next_phon_str not in ["n", "m", "N"] and phon[-1] != "#" and not (phon.strip("#") in vowels and next_phon_str == "@" and num_rem_syls <= 2 and final_n) and not (phon.strip("#") not in vowels and next_phon_str == "@" and num_rem_syls <= 1 and final_n):  # exclude e.g. [j] in koeien
                prop_used = 0.7
                prop_dur = int_dur * prop_used
                used_e = round(segments.intervals[seg_index].maxTime, 3)
                used_s = used_e - prop_dur
            else:
                prop_used = 0.4
                prop_dur = int_dur * prop_used
                used_s = round(segments.intervals[seg_index].minTime, 3) + (int_dur - prop_dur) / 2
                used_e = used_s + prop_dur
            x1 = used_s - frame_start
            x1 = 0 if x1 <= 0 else window if x1 > window else x1
            x2 = frame_end - used_e
            x2 = 0 if x2 <= 0 else window if x2 > window else x2
            prop_f_in_used_i = (window - (x1 + x2)) / window
            if prop_f_in_used_i <= 0.5:
                return -1
            else:
                return 0
    else:  # feature == "~"
        # lets see if current segment is neighboured by syllables
        # containing nasals
        # look backwards first
        vowel_found = False
        preceding_nasal = False
        following_nasal = False
        search_i = seg_index
        while not (vowel_found or preceding_nasal) and search_i > 0:
            search_i -= 1
            search_seg = segments.intervals[search_i].mark.strip("#")
            if search_seg in vowels:
                vowel_found = True
            elif search_seg in ["n", "m", "N", "E2"]:
                preceding_nasal = True
        # check if onset or coda of vowel contains a nasal
        # onset: directly preceding nasal
        # coda: we don't need to check coda because we passed through
        if not (vowel_found or preceding_nasal):
            preceding_nasal = False
        elif vowel_found:
            vowel_onset = segments.intervals[search_i - 1].mark.strip("#") if search_i > 0 else None
            preceding_nasal = True if vowel_onset in ["n", "m", "N"] else False
            if not preceding_nasal:
                # also check ik vowel is final_n
                vowel_time = segments.intervals[search_i].minTime
                vowel_word_i = words.indexContaining(vowel_time + 0.001)
                vowel_word = words.intervals[vowel_word_i]
                prev_final_n = re.search(r'([^eo]e|ë)n$', vowel_word.mark)
                num_rem_syls = 0
                for rem_int in segments.intervals[search_i:]:
                    if rem_int.mark in vowels2:
                        num_rem_syls += 1
                    if rem_int.mark[-1] == "#":
                        break
                preceding_nasal = prev_final_n and num_rem_syls <= 1
        # now look forward,
        # but only if preceding_nasal == False to save time
        if not preceding_nasal:
            vowel_found = False
            search_i = seg_index
            while not (vowel_found or following_nasal) and search_i < len(segments.intervals) - 1:
                search_i += 1
                search_seg = segments.intervals[search_i].mark.strip("#")
                if search_seg in vowels:
                    vowel_found = True
                elif search_seg in ["n", "m", "N", "E2"]:
                    following_nasal = True
            # check if onset or coda of vowel contains a nasal
            # onset: we don't need to check onset because we passed thru
            # coda: directly neighbouring nasal or j/r/l followed by nasal
            if not (vowel_found or following_nasal):
                following_nasal = False
            elif vowel_found:
                vowel_coda = segments.intervals[search_i + 1].mark.strip("#") if search_i < len(segments.intervals) - 1 else None
                if vowel_coda in ["n", "m", "N"]:
                    following_nasal = True
                elif vowel_coda in ["j", "r", "l"]:
                    vowel_coda2 = segments.intervals[search_i + 2].mark.strip("#") if search_i + 1 < len(segments.intervals) - 1 else None
                    if vowel_coda2 in ["n", "m", "N"]:
                        following_nasal = True
                    else:
                        following_nasal = False
                else:
                    following_nasal = False
                if not following_nasal:
                    # also check if vowel is final_n
                    vowel_time = segments.intervals[search_i].minTime
                    vowel_word_i = words.indexContaining(vowel_time + 0.001)
                    vowel_word = words.intervals[vowel_word_i]
                    next_final_n = re.search(r'([^eo]e|ë)n$', vowel_word.mark)
                    num_rem_syls = 0
                    for rem_int in segments.intervals[search_i:]:
                        if rem_int.mark in vowels2:
                            num_rem_syls += 1
                        if rem_int.mark[-1] == "#":
                            break
                    following_nasal = next_final_n and num_rem_syls <= 1
        nearby_nasal = True if (preceding_nasal or following_nasal) else False
        # positive
        if phon in vowels and next_phon_str in ["n", "m", "N"]:  # be aware, this now includes schwa in bananen
            if prev_phon in ["n", "m", "N"] and not (final_n and next_phon[-1] == "#"):
                prop_used = 1
                prop_dur = int_dur * prop_used
                used_s = round(segments.intervals[seg_index].minTime, 3)
                used_e = used_s + prop_dur
            elif next_next_phon_str not in vowels + ["n", "m", "N", "SIL", "SPN", "", "j", "w", None] and not (final_n and next_phon[-1] == "#"):
                prop_used = 0.4
                prop_dur = int_dur * prop_used
                used_e = round(segments.intervals[seg_index].maxTime, 3)
                used_s = used_e - prop_dur
            else:   # e.g. first vowel in 'jenever'
                return -1
            x1 = used_s - frame_start
            x1 = 0 if x1 <= 0 else window if x1 > window else x1
            x2 = frame_end - used_e
            x2 = 0 if x2 <= 0 else window if x2 > window else x2
            prop_f_in_used_i = (window - (x1 + x2)) / window
            if prop_f_in_used_i <= 0.5:
                return -1
            else:
                return 1
        elif phon.strip("#") in ["n", "m", "N"]:
            if final_n and phon[-1] == "#":
                return -1
            else:
                if phon[-1] != "#" and next_phon in vowels and next_next_phon_str in ["n", "m", "N"] and not (final_n and next_next_phon[-1] == "#"):  # if this is a nasal before a vowel followed by a nasal
                    prop_used = 0.7
                    prop_dur = int_dur * prop_used
                    used_e = round(segments.intervals[seg_index].maxTime, 3)
                    used_s = used_e - prop_dur
                elif prev_phon in vowels and prev_prev_phon in ["n", "m", "N"]:  # if this is a nasal after a vowel after a nasal
                    prop_used = 0.7
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3)
                    used_e = used_s + prop_dur
                elif prev_phon in vowels and next_phon_str not in vowels + ["n", "m", "N", "SIL", "SPN", "", "j", "w", None]:  # if this is a nasal following a nasalized vowel and preceding a consonant
                    prop_used = 0.7
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3)
                    used_e = used_s + prop_dur
                else:  # if just a regular isolated nasal
                    prop_used = 0.4
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3) + (int_dur - prop_dur) / 2
                    used_e = used_s + prop_dur
                x1 = used_s - frame_start
                x1 = 0 if x1 <= 0 else window if x1 > window else x1
                x2 = frame_end - used_e
                x2 = 0 if x2 <= 0 else window if x2 > window else x2
                prop_f_in_used_i = (window - (x1 + x2)) / window
                if prop_f_in_used_i <= 0.5:
                    return -1
                else:
                    return 1
        # negative
        elif not (nearby_nasal or phon.strip("[]#") in ["n", "m", "N", "E2", "SPN"]):
            # check to see if phone is in word-final syllable
            num_rem_syls = 0
            for rem_int in segments.intervals[seg_index:]:
                if rem_int.mark in vowels2:
                    num_rem_syls += 1
                if rem_int.mark[-1] == "#":
                    break
            if phon.strip("#") in vowels + ["j", "l", "r", "w"] and next_phon_str in ["", "SIL"]:
                return -1
            elif phon.strip("#") == "@" and num_rem_syls <= 1 and final_n:
                return -1
            else:
                if prev_phon[-1] != "#" and not (next_phon_str in vowels + ["j", "l", "r", "w"] and next_next_phon_str in ["", "SIL"]):
                    prop_used = 1
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3)
                    used_e = used_s + prop_dur
                elif prev_phon[-1] != "#":
                    prop_used = 0.7
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3)
                    used_e = used_s + prop_dur
                elif not (next_phon_str in vowels + ["j", "l", "r", "w"] and next_next_phon_str in ["", "SIL"]):
                    prop_used = 0.7
                    prop_dur = int_dur * prop_used
                    used_e = round(segments.intervals[seg_index].maxTime, 3)
                    used_s = used_e - prop_dur
                else:
                    prop_used = 0.4
                    prop_dur = int_dur * prop_used
                    used_s = round(segments.intervals[seg_index].minTime, 3) + (int_dur - prop_dur) / 2
                    used_e = used_s + prop_dur
                x1 = used_s - frame_start
                x1 = 0 if x1 <= 0 else window if x1 > window else x1
                x2 = frame_end - used_e
                x2 = 0 if x2 <= 0 else window if x2 > window else x2
                prop_f_in_used_i = (window - (x1 + x2)) / window
                if prop_f_in_used_i <= 0.5:
                    return -1
                else:
                    return 0
        # exclude
        else:
            return -1


textgrid.textgrid.detectEncoding = lambda f: encoding

print("Indexing Bootstrap files...")

produce_output_tg = False

wavpaths = []
for fp in glob.glob(af_path + chunk_folder + "*.wav"):
    wavpaths.append(fp)

# wavpaths = wavpaths[2000:2010]

# wavpaths = ['/vol/tensusers/timzee/af_classification/training_chunks_en/o/o_nl_fn001001_101.717_104.572.wav']

running_cores = 0

num_cores = 63
num_lex_lines = len(wavpaths)
# num_lex_lines = 1000
core_dict = {}
for i in range(num_cores):
    core_dict[str(i + 1)] = {}
    core_dict[str(i + 1)]["start"] = int(num_lex_lines / num_cores) * i + 1
    if i + 1 != num_cores:
        core_dict[str(i + 1)]["end"] = int(num_lex_lines / num_cores) * (i + 1)
    else:
        core_dict[str(i + 1)]["end"] = num_lex_lines

num_cols_per_frame = (3 * 13) + len(features)
num_cols = (2 * frame_window + 1) * (3 * 13) + len(corpora) + len(features)


def makeData(from_file, to_file, core):
    for counter, wp in enumerate(wavpaths[from_file:to_file + 1], 1):
        rate, sig = scipy.io.wavfile.read(wp)
        np_mfcc = python_speech_features.mfcc(sig, rate, winlen=window, winstep=step)
        np_mfcc_d = python_speech_features.delta(np_mfcc, 2)
        np_mfcc_dd = python_speech_features.delta(np_mfcc_d, 2)
        np_mfcc_all = np.append(np.append(np_mfcc, np_mfcc_d, axis=1), np_mfcc_dd, axis=1)
#        print(np_mfcc_all.shape)
        wn = wp.split("/")[-1]
        # get corpus info
        if wn[0] in ["F", "M"]:
            corpus = "ifa"
        elif wn[0] == "D":
            corpus = "ifadv"
        elif wn[0] == "p":
            corpus = "ecsd"
        else:
            corpus = "cgn-" + wn[0]
        #
        sent_id = ".".join(wn.split(".")[:-1])
        print(core, counter, "/", to_file - from_file, sent_id)
        tg_path = af_path + chunk_folder + sent_id + ".TextGrid"
        tg = textgrid.TextGrid()
        with makeTempFile(tg_path) as tempf:
            tg.read(tempf.name)
        intervals = tg.tiers[2].intervals
        end_time = round(intervals[-1].maxTime, 3)
        start_time = round(intervals[0].minTime, 3)
        # get number of syllables for each word
        word_intervals = tg.tiers[0].intervals
        word_syls = []
        word_segs = []
        label_dictionary = {key: [] for key in features}
        for w in word_intervals:
            n_syls = 0
            word_start = round(w.minTime, 3)
            word_end = round(w.maxTime, 3)
            first_seg = tg.tiers[2].indexContaining(word_start + 0.001)
            assert first_seg == 0 or intervals[first_seg - 1].mark[-1] == "#"
            last_seg = tg.tiers[2].indexContaining(word_end - 0.001)
            assert intervals[last_seg].mark[-1] == "#"
            n_segs = last_seg + 1 - first_seg
            word_segs.append(n_segs)
            for seg in intervals[first_seg:last_seg + 1]:
                if seg.mark.strip("#") in vowels:
                    n_syls += 1
            word_syls.append(n_syls)
        assert len(word_syls) == len(word_intervals)
        comments = tg.tiers[1].intervals
#        print(start_time, end_time)
        classes = np.zeros((0, num_cols_per_frame))
        int_i = 0
        num_frames = np_mfcc_all.shape[0]
        useable_frame_indices = []
        for frame in range(1, num_frames + 1):
            frame_s = round(start_time + (frame - 1) * step, 3)
            frame_e = frame_s + window
            if frame_e > end_time:  # because '0' samples can be appended to sig so it can be divided by an integer of frames
                frame_e = end_time
            intvl = intervals[int_i]
            if frame_s < round(intvl.minTime, 3):
                print(frame_s, round(intvl.minTime, 3))
            assert frame_s >= round(intvl.minTime, 3)
            if frame_e <= round(intvl.maxTime, 3):
                # calculate the proportion of the frame that is within the useable centre of the interval
                # this needs to be done separately for the nasalization feature, which should be aligned with end of intervals
                # for feat in ["@", "n", "~"]
                # pass feat to getFeatureLabel
                # which returns labels for each feat
                # construct label_list after for loop completes
                label_list = []
                for feat in features:
                    label = getFeatureLabel(frame_s, frame_e, feat, int_i, tg.tiers[2], tg.tiers[0], word_syls, word_segs, comments)
                    label_list.append(label)
                    label_dictionary[feat].append(label)
                if sum(label_list) > -3:
                    useable_frame_indices.append(frame - 1)
                row = np.array([np.append(np_mfcc_all[frame - 1, ], label_list)])
                classes = np.append(classes, row, axis=0)
            else:
                assert frame_e > round(intvl.maxTime, 3)
                proportions = [(round(intvl.maxTime, 3) - frame_s, int_i)]
                new_int = intvl
                new_int_i = int_i
                next_int_i = int_i
                while frame_e > round(new_int.maxTime, 3):
                    new_int_i += 1
                    new_int = intervals[new_int_i]
                    overlap = (frame_e - round(new_int.minTime, 3)) if frame_e <= round(new_int.maxTime, 3) else (round(new_int.maxTime, 3) - round(new_int.minTime, 3))
                    proportions.append((overlap, new_int_i))
                    if (frame_s + step) >= round(new_int.minTime, 3):
                        next_int_i = new_int_i
                best_int_i = max(proportions)[1]
                # calculate the proportion of the frame that is within the useable centre of the interval
                label_list = []
                for feat in features:
                    label = getFeatureLabel(frame_s, frame_e, feat, best_int_i, tg.tiers[2], tg.tiers[0], word_syls, word_segs, comments)
                    label_list.append(label)
                    label_dictionary[feat].append(label)
                if sum(label_list) > -3:
                    useable_frame_indices.append(frame - 1)
                row = np.array([np.append(np_mfcc_all[frame - 1, ], label_list)])
                classes = np.append(classes, row, axis=0)
                int_i = next_int_i
#        print(useable_frame_indices)
        for old_row in range(classes.shape[0]):
            if (old_row >= 2 * frame_window) and ((old_row - frame_window) in useable_frame_indices):
                new_labels = classes[old_row - frame_window, num_cols_per_frame - len(features):]
                new_feat = classes[old_row - (2 * frame_window):old_row + 1, :num_cols_per_frame - len(features)].flatten()
                new_row = np.array([np.append(np.append(new_feat, corpora[corpus]), new_labels)])
                # samples = np.append(samples, new_row, axis=0)
                with open(af_path + "AF_en" + str(int(core) + running_cores) + ".csv", "a") as f:
                    np.savetxt(f, new_row, fmt='%.5e', delimiter=",")
        if produce_output_tg:
            tg_out = textgrid.TextGrid(minTime=float(start_time))
            for af in features:
                tier = textgrid.IntervalTier(name=af, minTime=float(start_time))
                prev_class = -1
                min_time = float(start_time)
                max_time = min_time + (window - step) / 2 + step      # in reality we can't draw clear boundaries between labels; we would need a 0, 1, and -1 tier for each feature to show overlapping frames
                for frame_n, lab_cl in enumerate(label_dictionary[af], 1):
                    if prev_class != lab_cl:
                        if frame_n != 1:
                            tier.add(min_time, max_time, str(prev_class))
                            min_time = max_time
                    max_time = float(start_time) + (window - step) / 2 + frame_n * step
                    if len(label_dictionary[af]) == frame_n:
                        max_time += (window - step) / 2
                        tier.add(min_time, max_time, str(lab_cl))
                        break
                    prev_class = lab_cl
                tg_out.append(tier)
            with open(af_path + "en_training_labels_tgs/" + sent_id + ".TextGrid", "w") as f:
                tg_out.write(f)


print("Making data files...")

jobs = []
for core in range(num_cores):
    core_n = str(core + 1)
    f_file = core_dict[core_n]["start"] - 1
    t_file = core_dict[core_n]["end"] - 1
    p = multiprocessing.Process(target=makeData, args=[f_file, t_file, core_n])
    jobs.append(p)
    p.start()

for job in jobs:
    job.join()

print("Merging files...")

with open(af_path + "Bootstrap_en_" + str(frame_window) + "c_o.csv", "w") as f:
    all_samples = np.zeros((0, num_cols))
#    print(all_samples.shape)
    for c in core_dict:
        with open(af_path + "AF_en" + str(int(c) + running_cores) + ".csv", "r") as g:
            smpl = np.loadtxt(g, delimiter=",")
        print(smpl.shape)
        if smpl.shape[0] != 0:
            all_samples = np.append(all_samples, smpl, axis=0)
        os.remove(af_path + "AF_en" + str(int(c) + running_cores) + ".csv")
    np.savetxt(f, all_samples, fmt='%.5e', delimiter=",")
