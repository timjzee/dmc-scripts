import sys
import os
import glob
import scipy.io.wavfile
import python_speech_features
import tempfile
import textgrid
import numpy as np
import multiprocessing

# tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"
af_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"
#scratch = "/scratch/timzee/"

chunk_folder = "training_chunks/d/"
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

features = {
    "s": {"True": 1, "False": 0}
}

phones = {
    "@": {"s": "False"},
    "A": {"s": "False"},
    "AU": {"s": "False"},
    "E": {"s": "False"},
    "E2": {"s": "False"},
    "EI": {"s": "False"},
    "EU": {"s": "False"},
    "G": {"s": "False"},
    "I": {"s": "False"},
    "N": {"s": "False"},
    "O": {"s": "False"},
    "S": {"s": "False"},
    "[SIL]": {"s": "False"},
    "SIL": {"s": "False"},
    "[SPN]": {"s": "False"},
    "SPN": {"s": "False"},
    "": {"s": "False"},
    "U": {"s": "False"},
    "UI": {"s": "False"},
    "Z": {"s": "False"},
    "a": {"s": "False"},
    "b": {"s": "False"},
    "d": {"s": "False"},
    "e": {"s": "False"},
    "f": {"s": "False"},
    "g": {"s": "False"},
    "h": {"s": "False"},
    "i": {"s": "False"},
    "i:": {"s": "False"},
    "j": {"s": "False"},
    "k": {"s": "False"},
    "l": {"s": "False"},
    "m": {"s": "False"},
    "n": {"s": "False"},
    "o": {"s": "False"},
    "p": {"s": "False"},
    "r": {"s": "False"},
    "s": {"s": "True"},
    "t": {"s": "False"},
    "u": {"s": "False"},
    "v": {"s": "False"},
    "w": {"s": "False"},
    "x": {"s": "False"},
    "y": {"s": "False"},
    "z": {"s": "True"}
}

window = 0.025
step = 0.005
prop_used = 0.4     # proportion of each interval, centered around the middle of the interval, that is used

# number of preceding and subsequent frames used for training sample
# this creates a buffer at the start and end of the recording for which we have no training sample
# if frame_window = 5, and step = .005, we have a buffer of .025 ms
# we need to take this into account when we predict new data and construct the tg
frame_window = 15


def makeTempFile(fp):
    tempf = tempfile.NamedTemporaryFile()
    tempf.write(open(fp, "rb").read())
    tempf.flush()
    return tempf


def getFeatureLabel(phon):
    phon = phon.strip(" ")
#    print(phon)
    if phon in phones:
        label_list = [features[af][phones[phon][af]] for af in ["s"]]
        return label_list
#        return features[feature][phones[phon][feature]]
    else:
        return None


textgrid.textgrid.detectEncoding = lambda f: encoding

print("Indexing Bootstrap files...")

wavpaths = []
for fp in glob.glob(af_path + chunk_folder + "*.wav"):
    wavpaths.append(fp)

# wavpaths = wavpaths[:4]

# wavpaths = ['/vol/tensusers/timzee/af_classification/training_chunks/a_nl_fn007837_324.909_325.376.wav', '/vol/tensusers/timzee/af_classification/training_chunks/o_nl_fn001154_225.772_226.646.wav']

running_cores = 126

num_cores = 39
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
#    samples = np.zeros((0, num_cols))

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
        intervals = tg.tiers[0].intervals
        end_time = round(intervals[-1].maxTime, 3)
        start_time = round(intervals[0].minTime, 3)
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
                int_dur = round(intvl.duration(), 3)
                prop_dur = int_dur * prop_used
                used_s = round(intvl.minTime, 3) + (int_dur - prop_dur) / 2
                used_e = used_s + prop_dur
                x1 = used_s - frame_s
                x1 = 0 if x1 <= 0 else window if x1 > window else x1
                x2 = frame_e - used_e
                x2 = 0 if x2 <= 0 else window if x2 > window else x2
                prop_f_in_used_i = (window - (x1 + x2)) / window
#                print(prop_f_in_used_i, int_dur, used_s, used_e, frame_s, frame_e)
                if prop_f_in_used_i > 0.5:
                    useable_frame_indices.append(frame - 1)
                    label_list = getFeatureLabel(intvl.mark)
                else:
                    label_list = [99 for i in range(len(features))]
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
                best_int = intervals[best_int_i]
                # calculate the proportion of the frame that is within the useable centre of the interval
                int_dur = round(best_int.duration(), 3)
                prop_dur = int_dur * prop_used
                used_s = round(best_int.minTime, 3) + (int_dur - prop_dur) / 2
                used_e = used_s + prop_dur
                x1 = used_s - frame_s
                x1 = 0 if x1 <= 0 else window if x1 > window else x1
                x2 = frame_e - used_e
                x2 = 0 if x2 <= 0 else window if x2 > window else x2
                prop_f_in_used_i = (window - (x1 + x2)) / window
                if prop_f_in_used_i > 0.5:
                    useable_frame_indices.append(frame - 1)
                    label_list = getFeatureLabel(best_int.mark)
                else:
                    label_list = [99 for i in range(len(features))]
                row = np.array([np.append(np_mfcc_all[frame - 1, ], label_list)])
                classes = np.append(classes, row, axis=0)
                int_i = next_int_i
#        print(useable_frame_indices)
        for old_row in range(classes.shape[0]):
            if (old_row >= 2 * frame_window) and ((old_row - frame_window) in useable_frame_indices):
                new_labels = classes[old_row - frame_window, num_cols_per_frame - len(features):]
                if new_labels[0] < 90:
                    new_feat = classes[old_row - (2 * frame_window):old_row + 1, :num_cols_per_frame - len(features)].flatten()
                    new_row = np.array([np.append(np.append(new_feat, corpora[corpus]), new_labels)])
                    # samples = np.append(samples, new_row, axis=0)
                    with open(af_path + "AF_s" + str(int(core) + running_cores) + ".csv", "a") as f:
#                    with open(scratch + "AF_s" + core + ".csv", "a") as f:
                        np.savetxt(f, new_row, fmt='%.5e', delimiter=",")

#    with open(af_path + "AF_s" + core + ".csv", "w") as f:
#        np.savetxt(f, samples, delimiter=",")


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

with open(af_path + "Bootstrap_s_15c_d.csv", "w") as f:
#with open(scratch + "Bootstrap_s_large_a.csv", "w") as f:
    all_samples = np.zeros((0, num_cols))
#    print(all_samples.shape)
    for c in core_dict:
        with open(af_path + "AF_s" + str(int(c) + running_cores) + ".csv", "r") as g:
#        with open(scratch + "AF_s" + c + ".csv", "r") as g:
            smpl = np.loadtxt(g, delimiter=",")
        print(smpl.shape)
        if smpl.shape[0] != 0:
            all_samples = np.append(all_samples, smpl, axis=0)
        os.remove(af_path + "AF_s" + str(int(c) + running_cores) + ".csv")
#        os.remove(scratch + "AF_s" + c + ".csv")
    np.savetxt(f, all_samples, fmt='%.5e', delimiter=",")
