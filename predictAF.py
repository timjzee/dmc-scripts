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
    "Manner": {1: "plosive", 2: "fricative", 3: "nasal", 4: "glide", 5: "liquid", 6: "vowel", 7: "retroflex", 0: "nil"},
    "Place": {1: "bilabial", 2: "labiodental", 3: "alveolar", 4: "velar", 5: "glottal", 6: "palatal", 0: "nil"},
    "Voice": {1: "voiced", 2: "voiceless", 0: "nil"},
    "Backness": {1: "front", 2: "central", 3: "back", 0: "nil"},
    "Height": {1: "high", 2: "mid", 3: "low", 0: "nil"},
    "Rounding": {1: "round", 2: "spread", 0: "nil"},
    "Vowel length": {1: "long", 2: "short", 3: "diphthong", 0: "nil"},
}

expected_features = {"Manner": 2, "Place": 3, "Voice": 2}

_CSV_COLUMNS = ["frame" + str(i) + "mfcc" + str(j) for i in range(1, 12) for j in range(1, 40)]

window = 0.025
step = 0.005

frame_window = 5

num_feat = (2 * frame_window + 1) * (3 * 13)
num_feat_per_frame = (3 * 13)


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
    "Manner": tf.estimator.DNNClassifier(feature_columns=feature_columns, n_classes=8, hidden_units=[300, 150], model_dir=tens_path + "Manner_model"),
    "Place": tf.estimator.DNNClassifier(feature_columns=feature_columns, n_classes=7, hidden_units=[200], model_dir=tens_path + "Place_model"),
    "Voice": tf.estimator.DNNClassifier(feature_columns=feature_columns, n_classes=3, hidden_units=[100], model_dir=tens_path + "Voice_model")
}


file_paths = glob.glob(tens_path + "pred_fragments/*.wav")
file_paths = [
    "/vol/tensusers/timzee/af_classification/pred_fragments/fn001107_1_149.402_149.832.wav",
    "/vol/tensusers/timzee/af_classification/pred_fragments/fn001107_1_185.039_185.659.wav",
    "/vol/tensusers/timzee/af_classification/pred_fragments/fn001107_1_35.097_35.547.wav",
    "/vol/tensusers/timzee/af_classification/pred_fragments/fn001338_1_236.670_237.140.wav"
#    "/vol/tensusers/timzee/af_classification/pred_fragments/DVA10O_1_101.273_101.523.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/DVA10O_1_10.314_10.604.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/DVA10O_1_103.925_104.325.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/DVA10O_1_108.475_108.725.wav",
#    "/vol/tensusers/timzee/af_classification/pred_fragments/DVA10O_1_108.605_108.835.wav"
]

for fp in file_paths:
    fragment_id = ".".join(fp.split(".")[:-1]).split("/")[-1]
    wav, chan, start_time, end_time = fragment_id.split("_")
    print(fragment_id)
    rate, sig = scipy.io.wavfile.read(fp)
    np_mfcc = python_speech_features.mfcc(sig, rate, winlen=window, winstep=step)
    np_mfcc_d = python_speech_features.delta(np_mfcc, 2)
    np_mfcc_dd = python_speech_features.delta(np_mfcc_d, 2)
    np_mfcc_all = np.append(np.append(np_mfcc, np_mfcc_d, axis=1), np_mfcc_dd, axis=1)
    unseen_samples = np.zeros((0, num_feat))
    for old_row in range(np_mfcc_all.shape[0]):
        if old_row >= 2 * frame_window:
            new_feat = np_mfcc_all[old_row - (2 * frame_window):old_row + 1, :num_feat_per_frame].flatten()
            unseen_samples = np.append(unseen_samples, np.array([new_feat]), axis=0)
    # in final implementation also calculate deltas
    pd_samples = pd.DataFrame(data=unseen_samples, columns=_CSV_COLUMNS)
    predict_test_input_fn = create_predict_input_fn(pd_samples, batch_size=50)
    tg = textgrid.TextGrid(minTime=float(start_time))
    for af in ["Manner", "Place", "Voice"]:
        classifier = classifiers[af]
        classifier_output = list(classifier.predict(input_fn=predict_test_input_fn))
        predictions = np.array([item['class_ids'][0] for item in classifier_output])
        print(predictions)
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
            print(af_probs)
            with open(tens_path + "pred_textgrids/" + fragment_id + "_" + af + ".PitchTier", "w") as f:
                f.write('File type = "ooTextFile"\nObject class = "PitchTier"\n\n{}\n{}\n{}\n'.format(start_time, end_time, len(af_probs)))
                for frame_i, prob in enumerate(af_probs, 0):
                    f_time = min_time + frame_i * step
                    f.write('{}\n{}\n'.format(f_time, prob))
        for frame_n, pred_cl in enumerate(predictions, 1):
            print(frame_n, pred_cl, prev_class, min_time, max_time, end_time, features[af][pred_cl])
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
    with open(tens_path + "pred_textgrids/" + fragment_id + ".TextGrid", "w") as f:
        tg.write(f)


# test_dataframe = pd.read_csv(tens_path + "AF_manner_sample_test.csv", sep=",", header=None)
# test_targets, test_examples = parse_labels_and_features(test_dataframe)
# test_examples.describe()
# predict_test_input_fn = create_predict_input_fn(test_examples, test_targets, batch_size=100)
# test_predictions = classifier.predict(input_fn=predict_test_input_fn)
# test_predictions = np.array([item['class_ids'][0] for item in test_predictions])
# accuracy = metrics.accuracy_score(test_targets, test_predictions)
# print("Accuracy on test data: %0.2f" % accuracy)
