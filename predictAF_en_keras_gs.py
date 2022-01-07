import tensorflow as tf
import sys
import os
import numpy as np
import scipy.io.wavfile
import python_speech_features
import glob
import textgrid


if len(sys.argv) > 1:
    network_type = str(sys.argv[1])
    context_frames = str(sys.argv[2])
else:
    network_type = "BLSTM"
    context_frames = "15"

tens_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"

model_locations = {
    "BLSTM": {
        "5": {
            "16k": {
                "schwa": ["en_5c_16k_BLSTM_old5/", "/run-0"],
                "nasal": ["en_5c_16k_BLSTM_old5/", "/run-0"],
                "nasalization": ["en_5c_16k_BLSTM_old5/", "/run-0"]
            },
            "8k": "en_5c_8k/run-4"
        },
        "15": {
            "16k": {
                "schwa": ["en_15c_16k_BLSTM/", "/run-0"],
                "nasal": ["en_15c_16k_BLSTM/", "/run-0"],
                "nasalization": ["en_15c_16k_BLSTM/", "/run-0"]
            },
            "8k": "en_15c_8k/run-6"
        }
    },
    "FFNN": {
        "5": {
            "16k": {
                "schwa": ["en_5c_16k_FFNN/", "/run-0"],
                "nasal": ["en_5c_16k_FFNN/", "/run-0"],
                "nasalization": ["en_5c_16k_FFNN/", "/run-0"]
            },
            "8k": {
                "schwa": ["en_5c_8k_nn/", "/run-0"],
                "nasal": ["en_5c_8k_nn/", "/run-0"],
                "nasalization": ["en_5c_8k_nn/", "/run-0"]
            }
        },
        "15": {
            "16k": {
                "schwa": ["en_15c_16k_FFNN/", "/run-0"],
                "nasal": ["en_15c_16k_FFNN/", "/run-0"],
                "nasalization": ["en_15c_16k_FFNN/", "/run-0"]
            },
            "8k": {
                "schwa": ["en_15c_8k_nn/", "/run-0"],
                "nasal": ["en_15c_8k_nn/", "/run-0"],
                "nasalization": ["en_15c_8k_nn/", "/run-0"]
            }
        }
    }
}

features = ["schwa", "nasal", "nasalization"]
sampling_freqs = ["16k"]

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

corp = ["cgn-a", "cgn-c", "cgn-d", "cgn-k", "cgn-o", "ifadv", "ecsd", "ifa"]

window = 0.025
step = 0.005

frame_window = int(context_frames)

num_feat = (2 * frame_window + 1) * (3 * 13)  # + len(corpora)
num_feat_per_frame = (3 * 13)

print("Loading Classifiers...\n")
classifiers = {feat: {freq: tf.keras.models.load_model(tens_path + "keras_models/" + feat.join(model_locations[network_type][context_frames][freq][feat])) for freq in sampling_freqs} for feat in features}

# classifiers = {
#     "schwa": {"16k": tf.keras.models.load_model(tens_path + "keras_models/" + "schwa".join(model_locations[network_type][context_frames]["16k"]["schwa"]))},
#     "nasal": {"16k": tf.keras.models.load_model(tens_path + "keras_models/" + "nasal".join(model_locations[network_type][context_frames]["16k"]["nasal"]))},
#     "nasalization": {"16k": tf.keras.models.load_model(tens_path + "keras_models/" + "nasalization".join(model_locations[network_type][context_frames]["16k"]["nasalization"]))}
# }

frag_fol = "eval2"
file_paths = glob.glob(tens_path + "pred_fragments_en/" + frag_fol + "/*.wav")

# file_paths = [tens_path + "pred_fragments/af_eval_s/DVA10O_1_36.916_37.356.wav"]

# file_paths = [
#     tens_path + "pred_fragments/af_eval_s/fn000784_2_588.886_589.366.wav",
#     tens_path + "pred_fragments/af_eval_s/ppX5_X6_part_09_S_2_139.707_140.137.wav",
#     tens_path + "pred_fragments/af_eval_s/fn000404_1_207.644_208.184.wav",
#     tens_path + "pred_fragments/af_eval_s/fn006968_1_49.479_49.949.wav",
#     tens_path + "pred_fragments/af_eval_s/fn000724_1_430.181_430.641.wav",
#     tens_path + "pred_fragments/af_eval_s/fn001509_1_154.551_155.031.wav",
#     tens_path + "pred_fragments/af_eval_s/fn001297_1_130.002_130.462.wav",
#     tens_path + "pred_fragments/af_eval_s/fn000434_1_440.023_440.503.wav",
#     tens_path + "pred_fragments/af_eval_s/fn002965_1_1.065_1.525.wav",
#     tens_path + "pred_fragments/af_eval_s/fn006777_1_30.569_31.029.wav",
#     tens_path + "pred_fragments/af_eval_s/fn008090_1_208.582_209.072.wav",
#     tens_path + "pred_fragments/af_eval_s/fn001560_1_125.731_126.211.wav",
#     tens_path + "pred_fragments/af_eval_s/DVA13U_1_627.522_628.042.wav"
# ]

# remove already predicted fragments

tg_fol = frag_fol if len(frag_fol) > 1 else "cgn-" + frag_fol
if not os.path.exists(tens_path + "pred_textgrids_keras_en/" + tg_fol):
    os.mkdir(tens_path + "pred_textgrids_keras_en/" + tg_fol)
tg_fol += "/" + network_type + "_" + context_frames
if not os.path.exists(tens_path + "pred_textgrids_keras_en/" + tg_fol):
    os.mkdir(tens_path + "pred_textgrids_keras_en/" + tg_fol)

tg_paths = glob.glob(tens_path + "pred_textgrids_keras_en/" + tg_fol + "/*")

for tg_path in tg_paths:
    os.remove(tg_path)

# running_cores = 0
#
# num_cores = 1
# num_index_lines = len(file_paths)
# core_dict = {}
# for i in range(num_cores):
#     core_dict[str(i + 1)] = {}
#     core_dict[str(i + 1)]["start"] = int(num_index_lines / num_cores) * i + 1
#     if i + 1 != num_cores:
#         core_dict[str(i + 1)]["end"] = int(num_index_lines / num_cores) * (i + 1)
#     else:
#         core_dict[str(i + 1)]["end"] = num_index_lines


# def predict_files(core_num="", start_line=1, end_line=num_index_lines):
#    for fp in file_paths[start_line - 1:end_line]:
for fp in file_paths:
    fragment_id = ".".join(fp.split(".")[:-1]).split("/")[-1]
    print(fragment_id)
    # get corpus info
    if fragment_id[0] in ["F", "M"]:
        corpus = "ifa"
    elif fragment_id[0] == "D":
        corpus = "ifadv"
    elif fragment_id[0] == "p":
        corpus = "ecsd"
    elif fragment_id[:2] == "fn":
#        corpus = "cgn-" + frag_fol
#            corpus = "cgn-" + fragment_id[0]
        fn_num = int(fragment_id.split("_")[0][2:-1])
        if fn_num >= 100 and fn_num <= 159:
            corpus = "cgn-o"
        elif fn_num >= 800 and fn_num <= 839:
            corpus = "cgn-c"
        elif fn_num >= 670 and fn_num <= 703:
            corpus = "cgn-d"
        elif (fn_num >= 20 and fn_num <= 99) or (fn_num >= 780 and fn_num <= 799) or (fn_num >= 840 and fn_num <= 859):
            corpus = "cgn-a"
        else:
            corpus = "cgn-k"
    sample_freq = "8k" if corpus in ["cgn-c", "cgn-d"] else "16k"
    fragment_id_split = fragment_id.split("_")
    chan, start_time, end_time = fragment_id_split[-3:]
#        wav, chan, start_time, end_time = fragment_id.split("_")
    rate, sig = scipy.io.wavfile.read(fp)
    np_mfcc = python_speech_features.mfcc(sig, rate, winlen=window, winstep=step)
    np_mfcc_d = python_speech_features.delta(np_mfcc, 2)
    np_mfcc_dd = python_speech_features.delta(np_mfcc_d, 2)
    np_mfcc_all = np.append(np.append(np_mfcc, np_mfcc_d, axis=1), np_mfcc_dd, axis=1)
    unseen_samples = np.zeros((0, num_feat))
    for old_row in range(np_mfcc_all.shape[0]):
        if old_row >= 2 * frame_window:
            new_feat = np_mfcc_all[old_row - (2 * frame_window):old_row + 1, :num_feat_per_frame].flatten()
#                new_row = np.array([np.append(new_feat, corpora[corpus])])
#                print(unseen_samples.shape, np.array(new_row).shape)
            unseen_samples = np.append(unseen_samples, [new_feat], axis=0)
    if network_type == "BLSTM":
        unseen_samples = unseen_samples.reshape((unseen_samples.shape[0], (frame_window * 2 + 1), 13 * 3))
    # save unseen samples for inspection
#        summary_text = pd.DataFrame(np_mfcc).describe()
#        summary_text.to_csv(tens_path + "pred_mfcc_keras/" + corpus + "/" + fragment_id + "_sum.csv", float_format="%.2f")
#        pd_samples = pd.DataFrame(data=unseen_samples, columns=_CSV_COLUMNS)
#        predict_test_input_fn = create_predict_input_fn(pd_samples, batch_size=batch_size)
    tg = textgrid.TextGrid(minTime=float(start_time))
    for af in features:
        classifier = classifiers[af][sample_freq]
        probabilities = [float(i) for i in classifier.predict(unseen_samples)]
        predictions = [round(i) for i in probabilities]
        print(af, probabilities)
        # construct textgrids
        tier = textgrid.IntervalTier(name=af, minTime=float(start_time))
        prev_class = 99
        min_time = float(start_time)
        max_time = min_time + frame_window * step
        tier.add(min_time, max_time, "")    # add the empty interval for the initial buffer (due to windowing over preceding 5 frames)
        min_time = max_time
        # create 'probability' tier
        with open(tens_path + "pred_textgrids_keras_en/" + tg_fol + "/" + fragment_id + "_" + af + ".IntensityTier", "w") as f:
            f.write('File type = "ooTextFile"\nObject class = "IntensityTier"\n\n{}\n{}\n{}\n'.format(start_time, end_time, len(probabilities)))
            for frame_i, prob in enumerate(probabilities, 0):
                f_time = min_time + window / 2 + frame_i * step
                f.write('{}\n{}\n'.format(f_time, prob))
        for frame_n, pred_cl in enumerate(predictions, 1):
            if prev_class != pred_cl:
                if frame_n != 1:
                    tier.add(min_time, max_time, str(prev_class))
                    min_time = max_time
            max_time = float(start_time) + (frame_window * step) + (window - step) / 2 + frame_n * step
#                if max_time > float(end_time):
#                    max_time = float(end_time)
            if len(predictions) == frame_n:  # or max_time == float(end_time):
                max_time += (window - step) / 2
                tier.add(min_time, max_time, str(pred_cl))
                break
            prev_class = pred_cl
        if max_time < float(end_time):  # add the empty interval for the final buffer (due to windowing over subsequent 5 frames)
            tier.add(max_time, float(end_time), "")
        tg.append(tier)
    with open(tens_path + "pred_textgrids_keras_en/" + tg_fol + "/" + fragment_id + ".TextGrid", "w") as f:
        tg.write(f)


# jobs = []
# for core in range(num_cores):
#     core_n = str(core + 1)
#     s_line = core_dict[core_n]["start"]
#     e_line = core_dict[core_n]["end"]
#     core_n = str(running_cores + core + 1)
#     p = multiprocessing.Process(target=predict_files, args=[core_n, s_line, e_line])
#     jobs.append(p)
#     p.start()
#
# for job in jobs:
#     job.join()
