import tensorflow as tf
from tensorflow.python.data import Dataset
import sys
import pandas as pd
import numpy as np
import scipy.io.wavfile
import python_speech_features
import glob
import textgrid


tens_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"

features = {
    "s": {1: "True", 0: "False"}
}

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

expected_features = {"s": 1}

_CSV_COLUMNS = ["frame" + str(i) + "mfcc" + str(j) for i in range(1, 12) for j in range(1, 40)] + corp

window = 0.025
step = 0.005

frame_window = 5

num_feat = (2 * frame_window + 1) * (3 * 13) + len(corpora)
num_feat_per_frame = (3 * 13)

batch_size = 100


def create_predict_input_fn(features, batch_size):
    """A custom input_fn for sending mnist data to the estimator for predictions.

    Args:
      features: The features to base predictions on.
      labels: The labels of the prediction examples.

    Returns:
      A function that returns features for predictions.
    """
    def _input_fn():
        # raw_features = {"MFCCs": features.values}
        ds = Dataset.from_tensor_slices((features.to_dict('list')))  # warning: 2GB limit
        ds = ds.batch(batch_size)
        # Return the next batch of data.
        feature_batch = ds.make_one_shot_iterator().get_next()
        return feature_batch
    return _input_fn


feature_columns = [tf.feature_column.numeric_column(i) for i in _CSV_COLUMNS]
classifiers = {
    "s": tf.estimator.DNNClassifier(feature_columns=feature_columns, n_classes=2, hidden_units=[300], model_dir=tens_path + "s_8k_model")
}

frag_fol = "d"
file_paths = glob.glob(tens_path + "pred_fragments/" + frag_fol + "/*.wav")
# file_paths = [
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn001107_1_149.402_149.832.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn001107_1_185.039_185.659.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn001107_1_35.097_35.547.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn001338_1_236.670_237.140.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn000254_2_412.278_412.848.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn000254_2_983.158_983.618.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn000261_2_417.028_417.518.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn000265_2_233.057_233.487.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/af_eval/fn000265_2_59.917_60.377.wav"
#    "/vol/tensusers/timzee/af_classification/pred_fragments/ifadv/DVA10O_1_101.173_101.623.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/ifadv/DVA10O_1_10.214_10.704.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/ifadv/DVA10O_1_103.825_104.425.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/ifadv/DVA10O_1_108.375_108.825.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/ifadv/DVA10O_1_108.505_108.935.wav"
# ]

for fp in file_paths:
    fragment_id = ".".join(fp.split(".")[:-1]).split("/")[-1]
    # get corpus info
    if fragment_id[0] in ["F", "M"]:
        corpus = "ifa"
    elif fragment_id[0] == "D":
        corpus = "ifadv"
    elif fragment_id[0] == "p":
        corpus = "ecsd"
    else:
        corpus = "cgn-" + frag_fol
#        corpus = "cgn-" + fragment_id[0]
#        fn_num = int(fragment_id.split("_")[0][2:])
#        corpus = "cgn-o" if fn_num > 1000 and fn_num < 1566 else "cgn-a"
    #
    print(fragment_id)
    fragment_id_split = fragment_id.split("_")
    wav = "_".join(fragment_id_split[:-3])
    chan, start_time, end_time = fragment_id_split[-3:]
#    wav, chan, start_time, end_time = fragment_id.split("_")
    rate, sig = scipy.io.wavfile.read(fp)
    np_mfcc = python_speech_features.mfcc(sig, rate, winlen=window, winstep=step)
    np_mfcc_d = python_speech_features.delta(np_mfcc, 2)
    np_mfcc_dd = python_speech_features.delta(np_mfcc_d, 2)
    np_mfcc_all = np.append(np.append(np_mfcc, np_mfcc_d, axis=1), np_mfcc_dd, axis=1)
    unseen_samples = np.zeros((0, num_feat))
    for old_row in range(np_mfcc_all.shape[0]):
        if old_row >= 2 * frame_window:
            new_feat = np_mfcc_all[old_row - (2 * frame_window):old_row + 1, :num_feat_per_frame].flatten()
            new_row = np.array([np.append(new_feat, corpora[corpus])])
#            print(unseen_samples.shape, np.array(new_row).shape)
            unseen_samples = np.append(unseen_samples, np.array(new_row), axis=0)
    # save unseen samples for inspection
    summary_text = pd.DataFrame(np_mfcc).describe()
    summary_text.to_csv(tens_path + "pred_mfcc/" + corpus + "/" + fragment_id + "_sum.csv", float_format="%.2f")
    pd_samples = pd.DataFrame(data=unseen_samples, columns=_CSV_COLUMNS)
    predict_test_input_fn = create_predict_input_fn(pd_samples, batch_size=batch_size)
    tg = textgrid.TextGrid(minTime=float(start_time))
    for af in ["s"]:
        classifier = classifiers[af]
        classifier_output = list(classifier.predict(input_fn=predict_test_input_fn))
        predictions = np.array([item['class_ids'][0] for item in classifier_output])
        print(af, predictions)
        probabilities = np.array([item['probabilities'] for item in classifier_output])
        # construct textgrids
        tier = textgrid.IntervalTier(name=af, minTime=float(start_time))
        prev_class = 99
        min_time = float(start_time)
        max_time = min_time + frame_window * step
        tier.add(min_time, max_time, "")    # add the empty interval for the initial buffer (due to windowing over preceding 5 frames)
        min_time = max_time
        # create 'probability' tier
        if af in expected_features:
            prob_i = expected_features[af]
            af_probs = list(probabilities[:, prob_i])
#            print(af_probs)
            with open(tens_path + "pred_textgrids/" + corpus + "/" + fragment_id + "_" + af + ".IntensityTier", "w") as f:
                f.write('File type = "ooTextFile"\nObject class = "IntensityTier"\n\n{}\n{}\n{}\n'.format(start_time, end_time, len(af_probs)))
                for frame_i, prob in enumerate(af_probs, 0):
                    f_time = min_time + frame_i * step
                    f.write('{}\n{}\n'.format(f_time, prob))
        for frame_n, pred_cl in enumerate(predictions, 1):
#            print(frame_n, pred_cl, prev_class, min_time, max_time, end_time, features[af][pred_cl])
            if prev_class != pred_cl:
                if frame_n != 1:
                    tier.add(min_time, max_time, features[af][prev_class])
                    min_time = max_time
            max_time = float(start_time) + (frame_window * step) + frame_n * step
#            if max_time > float(end_time):
#                max_time = float(end_time)
            if len(predictions) == frame_n:  # or max_time == float(end_time):
                max_time += (window - step)
                tier.add(min_time, max_time, features[af][pred_cl])
                break
            prev_class = pred_cl
        if max_time < float(end_time):  # add the empty interval for the final buffer (due to windowing over subsequent 5 frames)
            tier.add(max_time, float(end_time), "")
        tg.append(tier)
    with open(tens_path + "pred_textgrids/" + corpus + "/" + fragment_id + "_" + af + ".TextGrid", "w") as f:
        tg.write(f)


# test_dataframe = pd.read_csv(tens_path + "AF_manner_sample_test.csv", sep=",", header=None)
# test_targets, test_examples = parse_labels_and_features(test_dataframe)
# test_examples.describe()
# predict_test_input_fn = create_predict_input_fn(test_examples, test_targets, batch_size=100)
# test_predictions = classifier.predict(input_fn=predict_test_input_fn)
# test_predictions = np.array([item['class_ids'][0] for item in test_predictions])
# accuracy = metrics.accuracy_score(test_targets, test_predictions)
# print("Accuracy on test data: %0.2f" % accuracy)
