import sys
import os
import glob
import scipy.io.wavfile
import python_speech_features
import tempfile
import textgrid
import numpy as np
import multiprocessing

tens_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"
af_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"

wav_folder = "SLspeech/sentences/hm/"
tg_folder = "SLcorpus/Labels/sentences/"

encoding = "latin-1"

features = {
    "s": {"True": 1, "False": 0}
}

phones = {
    "@": {"s": "False"},
    "A": {"s": "False"},
    "A+": {"s": "False"},
    "O+": {"s": "False"},
    "E": {"s": "False"},
    "E:": {"s": "False"},
    "E+": {"s": "False"},
    "2": {"s": "False"},
    "2:": {"s": "False"},
    "G": {"s": "False"},
    "I": {"s": "False"},
    "N": {"s": "False"},
    "O": {"s": "False"},
    "O:": {"s": "False"},
    "S": {"s": "False"},
    "sil": {"s": "False"},
    "SIL": {"s": "False"},
    "*": {"s": "False"},
    "": {"s": "False"},
    "#": {"s": "False"},
    "Y": {"s": "False"},
    "Y+": {"s": "False"},
    "Y:": {"s": "False"},
    "9": {"s": "False"},
    "9+": {"s": "False"},
    "9:": {"s": "False"},
    "Z": {"s": "False"},
    "a": {"s": "False"},
    "a:": {"s": "False"},
    "b": {"s": "False"},
    "d": {"s": "False"},
    "e": {"s": "False"},
    "e:": {"s": "False"},
    "f": {"s": "False"},
    "g": {"s": "False"},
    "h": {"s": "False"},
    "i": {"s": "False"},
    "i:": {"s": "False"},
    "j": {"s": "False"},
    "J": {"s": "False"},
    "k": {"s": "False"},
    "l": {"s": "False"},
    "m": {"s": "False"},
    "n": {"s": "False"},
    "o": {"s": "False"},
    "o:": {"s": "False"},
    "p": {"s": "False"},
    "r": {"s": "False"},
    "s": {"s": "True"},
    "t": {"s": "False"},
    "u": {"s": "False"},
    "u:": {"s": "False"},
    "v": {"s": "False"},
    "w": {"s": "False"},
    "x": {"s": "False"},
    "y": {"s": "False"},
    "y:": {"s": "False"},
    "z": {"s": "True"}
}

window = 0.025
step = 0.005

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


def getFeatureLabel(phon):
    phon = phon.strip(" ")
    if phon in phones:
        label_list = [features[af][phones[phon][af]] for af in ["s"]]
        return label_list
#        return features[feature][phones[phon][feature]]
    else:
        return None


textgrid.textgrid.detectEncoding = lambda f: encoding

print("Indexing IFA files...")

wavpaths = []
for speaker in os.listdir(tens_path + wav_folder):
    if os.path.isdir(tens_path + wav_folder + speaker):
        for fp in glob.glob(tens_path + wav_folder + speaker + "/*_16000.wav"):
            wavpaths.append(fp)

# wavpaths = ['/vol/tensusers/timzee/IFAcorpus/SLspeech/sentences/hm/F20N/F20N1FS37A_hm.wav', '/vol/tensusers/timzee/IFAcorpus/SLspeech/sentences/hm/F20N/F20N1FPA1ABC_hm.wav', '/vol/tensusers/timzee/IFAcorpus/SLspeech/sentences/hm/F20N/F20N1FPA1HVDA_hm.wav']

num_cores = 62
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
num_cols = (2 * frame_window + 1) * (3 * 13) + len(features)


def makeData(from_file, to_file, core):
    samples = np.zeros((0, num_cols))

    for counter, wp in enumerate(wavpaths[from_file:to_file + 1], 1):
        rate, sig = scipy.io.wavfile.read(wp)
        np_mfcc = python_speech_features.mfcc(sig, rate, winlen=window, winstep=step)
        np_mfcc_d = python_speech_features.delta(np_mfcc, 2)
        np_mfcc_dd = python_speech_features.delta(np_mfcc_d, 2)
        np_mfcc_all = np.append(np.append(np_mfcc, np_mfcc_d, axis=1), np_mfcc_dd, axis=1)
        spkr, wn = wp.split("/")[-2:]
        sent_id = wn.split("_")[0]
        print(core, counter, "/", to_file - from_file, sent_id)
        tg_match = tens_path + tg_folder + spkr + "/ASPEX/" + sent_id + "_*.aspex"
        transcriptions = glob.glob(tg_match)
        if len(transcriptions) > 0:
            tg_path = transcriptions[0]
        else:
            continue
        tg = textgrid.TextGrid()
        with makeTempFile(tg_path) as tempf:
            tg.read(tempf.name)
        intervals = tg.tiers[0].intervals
        end_time = intervals[-1].maxTime
        classes = np.zeros((0, num_cols_per_frame))
        int_i = 0
        num_frames = np_mfcc_all.shape[0]
        for frame in range(1, num_frames + 1):
            frame_s = (frame - 1) * step
            frame_e = frame_s + window
            if frame_e > end_time:  # because '0' samples can be appended to sig so it can be divided by an integer of frames
                frame_e = end_time
            int = intervals[int_i]
            assert frame_s >= int.minTime
            if frame_e <= int.maxTime:
                label_list = getFeatureLabel(int.mark)
                if label_list is not None:
                    row = np.array([np.append(np_mfcc_all[frame - 1, ], label_list)])
                    classes = np.append(classes, row, axis=0)
            else:
                assert frame_e > int.maxTime
                proportions = [(int.maxTime - frame_s, int_i)]
                new_int = int
                new_int_i = int_i
                next_int_i = int_i
                while frame_e > new_int.maxTime:
                    new_int_i += 1
                    new_int = intervals[new_int_i]
                    overlap = (frame_e - new_int.minTime) if frame_e <= new_int.maxTime else (new_int.maxTime - new_int.minTime)
                    proportions.append((overlap, new_int_i))
                    if (frame_s + step) >= new_int.minTime:
                        next_int_i = new_int_i
                best_int_i = max(proportions)[1]
                best_int = intervals[best_int_i]
                label_list = getFeatureLabel(best_int.mark)
                if label_list is None:
                    label_list = [99 for i in range(len(features))]
                row = np.array([np.append(np_mfcc_all[frame - 1, ], label_list)])
                classes = np.append(classes, row, axis=0)
                int_i = next_int_i
        for old_row in range(classes.shape[0]):
            if old_row >= 2 * frame_window:
                new_labels = classes[old_row - frame_window, num_cols_per_frame - len(features):]
                if new_labels[0] < 90:
                    new_feat = classes[old_row - (2 * frame_window):old_row + 1, :num_cols_per_frame - len(features)].flatten()
                    new_row = np.array([np.append(new_feat, new_labels)])
                    samples = np.append(samples, new_row, axis=0)

    with open(af_path + "AF_s" + core + ".csv", "w") as f:
        np.savetxt(f, samples, delimiter=",")


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

with open(af_path + "AF_s_16000.csv", "w") as f:
    all_samples = np.zeros((0, num_cols))
    print(all_samples.shape)
    for c in core_dict:
        with open(af_path + "AF_s" + c + ".csv", "r") as g:
            smpl = np.loadtxt(g, delimiter=",")
        print(smpl.shape)
        if smpl.shape[0] != 0:
            all_samples = np.append(all_samples, smpl, axis=0)
        os.remove(af_path + "AF_s" + c + ".csv")
    np.savetxt(f, all_samples, delimiter=",")
