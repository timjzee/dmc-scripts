import tensorflow as tf
from tensorflow.python.data import Dataset
import pandas as pd
import sys
import os
import glob
import math
import numpy as np
from matplotlib import pyplot as plt
from sklearn import metrics
import seaborn as sns
import time

t_start = time.time()

tens_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"

num_cores = 30

afs = ["schwa", "n"]
corpus = "16k"
corpora = ["cgn-a", "cgn-c", "cgn-d", "cgn-k", "cgn-o", "ifadv", "ecsd", "ifa"]

n_classes = {"n": 2, "schwa": 2}

feature_dict = {
    "n": {1: "True", 0: "False"},
    "schwa": {1: "True", 0: "False"}
}

af = "n"

_CSV_COLUMN_DEFAULTS = [[0.0] for i in range(429 + len(corpora) + len(afs))]
_CSV_COLUMNS = ["frame" + str(i) + "mfcc" + str(j) for i in range(1, 12) for j in range(1, 40)] + corpora + afs
# _CSV_COLUMNS = ["MFCCs" for i in range(1, 12) for j in range(1, 40)] + [af]

tf.logging.set_verbosity(tf.logging.ERROR)

pd.options.display.max_rows = 10
pd.options.display.float_format = '{:.1f}'.format

print("Loading data...")
train_data = pd.read_csv(tens_path + "Bootstrap_en_large_" + corpus + "_train_labs.csv", sep=",", header=None)
valid_data = pd.read_csv(tens_path + "Bootstrap_en_large_" + corpus + "_valid_labs.csv", sep=",", header=None)
train_n = train_data.shape[0]
# train_n = 20000
# assuming label is in final columns
feat_n = 429 + len(corpora)
# feat_n = len(_CSV_COLUMNS) - 1

periods = 1
batch_size = 100
num_steps = train_n // batch_size
num_steps = num_steps - (num_steps % periods)
print(num_steps, "steps in data with batch size of", batch_size)


def parse_labels_and_features(dataset):
    """Extracts labels and features.

    This is a good place to scale or transform the features if needed.

    Args:
      dataset: A Pandas `Dataframe`, containing the label on the final column and
        MFCC values on the remaining columns.
    Returns:
      A `tuple` `(labels, features)`:
        labels: A Pandas `Series`.
        features: A Pandas `DataFrame`.
    """
    num_cols = dataset.shape[1]
    lab_i = num_cols - (len(afs) - afs.index(af))
#    lab_i = num_cols - 1
    labels = dataset[lab_i].astype(int)
    features = dataset.iloc[:, :feat_n]
    return labels, features


def create_training_input_fn(data_file, batch_size, num_epochs=None, shuffle=False):
    """A custom input_fn for sending MNIST data to the estimator for training.

    Args:
      features: The training features.
      labels: The training labels.
      batch_size: Batch size to use during training.

    Returns:
      A function that returns batches of training features and labels during
      training.
    """
    def _input_fn(num_epochs=None, shuffle=False):
        # Input pipelines are reset with each call to .train(). To ensure model
        # gets a good sampling of data, even when number of steps is small, we
        # shuffle all the data before creating the Dataset object
        def parse_csv(value):
            tf.logging.info('Parsing {}'.format(data_file))
            columns = tf.decode_csv(value, record_defaults=_CSV_COLUMN_DEFAULTS)
            features = dict(zip(_CSV_COLUMNS, columns))
            for artfeat in afs:
                if artfeat != af:
                    features.pop(artfeat)
            labels = features.pop(af)
            classes = tf.cast(labels, tf.int32)  # convert to integers
            return features, classes
        # Extract lines from input files using the Dataset API.
        ds = tf.data.TextLineDataset(data_file)
        if shuffle:
            ds = ds.shuffle(train_n)
        ds = ds.map(parse_csv, num_parallel_calls=num_cores)
        # We call repeat after shuffling, rather than before, to prevent separate
        # epochs from blending together.
        ds = ds.batch(batch_size).repeat(num_epochs)
        # Return the next batch of data.
        feature_batch, label_batch = ds.make_one_shot_iterator().get_next()
        return feature_batch, label_batch
    return _input_fn


def create_predict_input_fn(data_file, batch_size):
    """A custom input_fn for sending mnist data to the estimator for predictions.

    Args:
      features: The features to base predictions on.
      labels: The labels of the prediction examples.

    Returns:
      A function that returns features and labels for predictions.
    """
    def _input_fn():
        def parse_csv(value):
            tf.logging.info('Parsing {}'.format(data_file))
            columns = tf.decode_csv(value, record_defaults=_CSV_COLUMN_DEFAULTS)
            features = dict(zip(_CSV_COLUMNS, columns))
            for artfeat in afs:
                if artfeat != af:
                    features.pop(artfeat)
            labels = features.pop(af)
            classes = tf.cast(labels, tf.int32)  # convert to integers
            return features, classes
        # Extract lines from input files using the Dataset API.
        ds = tf.data.TextLineDataset(data_file)
        ds = ds.map(parse_csv, num_parallel_calls=num_cores)
        ds = ds.batch(batch_size)
        # Return the next batch of data.
        feature_batch, label_batch = ds.make_one_shot_iterator().get_next()
        return feature_batch, label_batch
    return _input_fn


def train_nn_classification_model(learning_rate, steps, batch_size, hidden_units, training_file, validation_file):
    """Trains a neural network classification model for the MNIST digits dataset.

    In addition to training, this function also prints training progress information,
    a plot of the training and validation loss over time, and a confusion
    matrix.

    Args:
      learning_rate: A `float`, the learning rate to use.
      steps: A non-zero `int`, the total number of training steps. A training step
        consists of a forward and backward pass using a single batch.
      batch_size: A non-zero `int`, the batch size.
      hidden_units: A `list` of int values, specifying the number of neurons in each layer.
      training_examples: A `DataFrame` containing the training features.
      training_targets: A `DataFrame` containing the training labels.
      validation_examples: A `DataFrame` containing the validation features.
      validation_targets: A `DataFrame` containing the validation labels.

    Returns:
      The trained `DNNClassifier` object.
    """
    steps_per_period = steps / periods
    # Create the input functions.
    predict_training_input_fn = create_predict_input_fn(training_file, batch_size)
    predict_validation_input_fn = create_predict_input_fn(validation_file, batch_size)
    training_input_fn = create_training_input_fn(training_file, batch_size)
    # Create feature columns.
    # feature_columns = [tf.feature_column.numeric_column('MFCCs', shape=feat_n)]
    feature_columns = [tf.feature_column.numeric_column(i) for i in _CSV_COLUMNS[:-len(afs)]]
    # Create a DNNClassifier object.
    my_optimizer = tf.train.AdagradOptimizer(learning_rate=learning_rate)
    my_optimizer = tf.contrib.estimator.clip_gradients_by_norm(my_optimizer, 5.0)
    classifier = tf.estimator.DNNClassifier(feature_columns=feature_columns, n_classes=n_classes[af], hidden_units=hidden_units, optimizer=my_optimizer, config=tf.contrib.learn.RunConfig(keep_checkpoint_max=1), model_dir=tens_path + af + "_" + corpus + "_model")
    # Train the model, but do so inside a loop so that we can periodically assess
    # loss metrics.
    print("Training model...")
    print("LogLoss error (on validation data):")
    training_errors = []
    validation_errors = []
    for period in range(0, periods):
        # Train the model, starting from the prior state.
        classifier.train(input_fn=training_input_fn, steps=steps_per_period)
        # Take a break and compute probabilities.
        training_predictions = list(classifier.predict(input_fn=predict_training_input_fn))
        training_probabilities = np.array([item['probabilities'] for item in training_predictions])
        training_pred_class_id = np.array([item['class_ids'][0] for item in training_predictions])
        training_pred_one_hot = tf.keras.utils.to_categorical(training_pred_class_id, n_classes[af])
        validation_predictions = list(classifier.predict(input_fn=predict_validation_input_fn))
        validation_probabilities = np.array([item['probabilities'] for item in validation_predictions])
        validation_pred_class_id = np.array([item['class_ids'][0] for item in validation_predictions])
        validation_pred_one_hot = tf.keras.utils.to_categorical(validation_pred_class_id, n_classes[af])
        # Compute training and validation errors.
        training_log_loss = metrics.log_loss(training_targets, training_pred_one_hot, labels=[i for i in range(n_classes[af])])
        validation_log_loss = metrics.log_loss(validation_targets, validation_pred_one_hot, labels=[i for i in range(n_classes[af])])
        # Occasionally print the current loss.
        print("  period %02d : %0.2f" % (period, validation_log_loss))
        # Add the loss metrics from this period to our list.
        training_errors.append(training_log_loss)
        validation_errors.append(validation_log_loss)
    print("Model training finished.")
    # Remove event files to save disk space.
    _ = map(os.remove, glob.glob(os.path.join(classifier.model_dir, 'events.out.tfevents*')))
    # Calculate final predictions (not probabilities, as above).
    final_predictions = classifier.predict(input_fn=predict_validation_input_fn)
    final_predictions = np.array([item['class_ids'][0] for item in final_predictions])
    accuracy = metrics.accuracy_score(validation_targets, final_predictions)
    print("Final accuracy (on validation data): %0.2f" % accuracy)
    # Output a graph of loss metrics over periods.
    plt.ylabel("LogLoss")
    plt.xlabel("Periods")
    plt.title("LogLoss vs. Periods")
    plt.plot(training_errors, label="training")
    plt.plot(validation_errors, label="validation")
    plt.legend()
    plt.show()
    # Output a plot of the confusion matrix.
    cm = metrics.confusion_matrix(validation_targets, final_predictions)
    with open(tens_path + af + "_" + corpus + "_validation_cm.csv", "w") as f:
        np.savetxt(f, cm.astype(int), fmt='%d', delimiter=",", comments="", header=",".join([feature_dict[af][i] for i in range(n_classes[af])]))
    # Normalize the confusion matrix by row (i.e by the number of samples
    # in each class).
    cm_normalized = cm.astype("float") / cm.sum(axis=1)[:, np.newaxis]
    ax = sns.heatmap(cm_normalized, cmap="bone_r")
    ax.set_aspect(1)
    plt.title("Confusion matrix")
    plt.ylabel("True label")
    plt.xlabel("Predicted label")
    plt.show()
    return classifier


print("Preparing data...")
# training_targets, training_examples = parse_labels_and_features(train_data)
lab_i = afs.index(af)
training_targets = train_data[lab_i].astype(int)
# print(training_examples.describe())
# validation_targets, validation_examples = parse_labels_and_features(valid_data)
validation_targets = valid_data[lab_i].astype(int)
# print(validation_examples.describe())

print("Starting training...")
classifier = train_nn_classification_model(
    learning_rate=0.05,
    steps=num_steps,
    batch_size=batch_size,
    hidden_units=[300],
    training_file=tens_path + "Bootstrap_en_large_" + corpus + "_train.csv",
    validation_file=tens_path + "Bootstrap_en_large_" + corpus + "_valid.csv")

t_end = time.time()
print("Execution time: ", t_end - t_start)

# test_dataframe = pd.read_csv(tens_path + "AF_manner_sample_test.csv", sep=",", header=None)
# test_targets, test_examples = parse_labels_and_features(test_dataframe)
# test_examples.describe()
# predict_test_input_fn = create_predict_input_fn(test_examples, test_targets, batch_size=100)
# test_predictions = classifier.predict(input_fn=predict_test_input_fn)
# test_predictions = np.array([item['class_ids'][0] for item in test_predictions])
# accuracy = metrics.accuracy_score(test_targets, test_predictions)
# print("Accuracy on test data: %0.2f" % accuracy)

# inspect neurons

# print(classifier.get_variable_names())
# weights0 = classifier.get_variable_value("dnn/hiddenlayer_0/kernel")
# print("weights0 shape:", weights0.shape)
# num_nodes = weights0.shape[1]
# num_rows = int(math.ceil(num_nodes / 8.0))
# fig, axes = plt.subplots(num_rows, 8, figsize=(20, 2 * num_rows))
# for coef, ax in zip(weights0.T, axes.ravel()):
#    # Weights in coef is reshaped from 1x784 to 28x28.
#    ax.matshow(coef.reshape(13, 1), cmap=plt.cm.pink)
#    ax.set_xticks(())
#    ax.set_yticks(())
# plt.show()
