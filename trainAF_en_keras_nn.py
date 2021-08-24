import tensorflow as tf
# from tensorflow.keras.utils import plot_model
from tensorboard.plugins.hparams import api as hp
import pandas as pd
import sys
import numpy as np
# from matplotlib import pyplot as plt
import time
import os


os.nice(19)
# os.environ["CUDA_DEVICE_ORDER"] = "PCI_BUS_ID"
# os.environ["CUDA_VISIBLE_DEVICES"] = "1"  # use id from $ nvidia-smi


tens_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"
log_dir = tens_path + "tb_log/"
save_dir = tens_path + "keras_models/"
session_name = "en_5c_16k_nn"
training_file = "Bootstrap_en_5c_16k_train.csv"      # toy_s_train.csv Bootstrap_s_large_16k_train.csv
validation_file = "Bootstrap_en_5c_16k_valid.csv"    # toy_s_valid.csv Bootstrap_s_large_16k_valid.csv
test_file = "Bootstrap_en_5c_16k_test.csv"           # Bootstrap_s_large_16k_test.csv

context_size = 5
# mfcc_length = 429
mfcc_length = 13 * 3 * (context_size * 2 + 1)
corpora = ["cgn-a", "cgn-c", "cgn-d", "cgn-k", "cgn-o", "ifadv", "ecsd", "ifa"]
features = ["schwa", "nasal", "nasalization"]
# specify rows for each feature in dictionary
train_rows = {"schwa": 11826612, "nasal": 18660793, "nasalization": 11001891}   # 5c-16k: ???; 5c-8k: ???
valid_rows = {"schwa": 1478303, "nasal": 2330934, "nasalization": 1375814}   # 5c-16k: ???; 5c-8k: ???
test_rows = {"schwa": 1477467, "nasal": 2333210, "nasalization": 1375118}    # 5c-16k: ???; 5c-8k: ???

# batch_size = 1000
# learning_rate = 0.005
n_epochs = 5
classification_threshold = 0.5

# Establish the metrics the model will measure.
# METRICS = [
#     tf.keras.metrics.BinaryAccuracy(name='accuracy', threshold=classification_threshold),
#     tf.keras.metrics.Precision(name='precision', thresholds=classification_threshold),
#     tf.keras.metrics.Recall(name='recall', thresholds=classification_threshold)
# ]

HP_L_RATE = hp.HParam('learning_rate', hp.Discrete([0.001]))         # [0.001, 0.005]
HP_NUM_UNITS = hp.HParam('num_units', hp.Discrete([500]))                         # [500]
HP_I_W_DECAY = hp.HParam('i_weight_decay', hp.Discrete([0.0]))    # input/kernel weight decay   [0.0, 0.001]
HP_R_W_DECAY = hp.HParam('r_weight_decay', hp.Discrete([0.0]))    # recurrent weight decay      [0.0, 0.001]
HP_B_W_DECAY = hp.HParam('b_weight_decay', hp.Discrete([0.0]))    # bias weight decay           [0.0, 0.001]
HP_DROPOUT = hp.HParam('dropout', hp.Discrete([0.0]))                  # [0.0, 0.1]
HP_BATCH_SIZE = hp.HParam('batch_size', hp.Discrete([1000]))                 # [1000]


def generate_arrays_from_file(ftr, path, batchsize):
    """Load data."""
    inputs = []
    targets = []
    batchcount = 0
    while True:
        with open(path) as f:
            for line in f:
                line_list = line[:-1].split(',')
                # we should check the target for the relevant feature and if target == -1, it should be skipped
                feature_label_i = features.index(ftr) - len(features)
                if float(line_list[feature_label_i]) > -0.5:
                    inputs.append(line_list[:-len(features)])
                    targets.append(line_list[feature_label_i])
                    batchcount += 1
                    if batchcount > batchsize:
                        X = np.array(inputs, dtype='float32')
                        X = X[:, :-8]
                        y = np.array(targets, dtype='float32')
                        yield (X, y)
                        inputs = []
                        targets = []
                        batchcount = 0


# def plot_curve(epochs, hist, list_of_metrics):
#     """Plot a curve of one or more classification metrics vs. epoch."""
#     # list_of_metrics should be one of the names shown in:
#     # https://www.tensorflow.org/tutorials/structured_data/imbalanced_data#define_the_model_and_metrics
#     plt.figure()
#     plt.xlabel("Epoch")
#     plt.ylabel("Value")
#     for m in list_of_metrics:
#         x = hist[m]
#         plt.plot(epochs[1:], x[1:], label=m)
#     plt.legend()
#     plt.show()


def create_model(hparams):                                                      # previously my_metrics
    """Create and compile a deep neural net."""
    with mirrored_strategy.scope():
        model = tf.keras.models.Sequential()
        # Define the layers
        model.add(tf.keras.layers.Dropout(hparams[HP_DROPOUT]))
        dense_layer1 = tf.keras.layers.Dense(
            units=hparams[HP_NUM_UNITS],
            kernel_regularizer=tf.keras.regularizers.l2(hparams[HP_I_W_DECAY]),
            bias_regularizer=tf.keras.regularizers.l2(hparams[HP_B_W_DECAY]))
        model.add(dense_layer1)
        model.add(tf.keras.layers.Dropout(hparams[HP_DROPOUT]))
        dense_layer2 = tf.keras.layers.Dense(
            units=hparams[HP_NUM_UNITS],
            kernel_regularizer=tf.keras.regularizers.l2(hparams[HP_I_W_DECAY]),
            bias_regularizer=tf.keras.regularizers.l2(hparams[HP_B_W_DECAY]))
        model.add(dense_layer2)
        model.add(tf.keras.layers.Dropout(hparams[HP_DROPOUT]))
        dense_layer3 = tf.keras.layers.Dense(
            units=hparams[HP_NUM_UNITS],
            kernel_regularizer=tf.keras.regularizers.l2(hparams[HP_I_W_DECAY]),
            bias_regularizer=tf.keras.regularizers.l2(hparams[HP_B_W_DECAY]))
        model.add(dense_layer3)
        model.add(tf.keras.layers.Dense(units=1, activation='sigmoid'))
        my_metrics = [
            tf.keras.metrics.BinaryAccuracy(name='accuracy', threshold=classification_threshold),
            tf.keras.metrics.Precision(name='precision', thresholds=classification_threshold),
            tf.keras.metrics.Recall(name='recall', thresholds=classification_threshold)
        ]
    # Construct the layers into a model that TensorFlow can execute.
    model.compile(
        optimizer=tf.keras.optimizers.Adam(lr=hparams[HP_L_RATE]),    # my_learning_rate
        loss="binary_crossentropy",
        metrics=my_metrics)
    return model


def train_model(feature, r_name, hparams, model):
    """Train the model by feeding it data."""
    hp_log_dir = log_dir + session_name + "/" + feature + "/" + r_name
    history = model.fit(
        generate_arrays_from_file(feature, tens_path + training_file, hparams[HP_BATCH_SIZE]),
        steps_per_epoch=train_rows[feature] // hparams[HP_BATCH_SIZE], epochs=n_epochs,
        validation_data=generate_arrays_from_file(feature, tens_path + validation_file, hparams[HP_BATCH_SIZE]),
        validation_steps=valid_rows[feature] // hparams[HP_BATCH_SIZE],
        callbacks=[
            tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=1),
            tf.keras.callbacks.TensorBoard(log_dir=hp_log_dir, histogram_freq=1),  # log_dir
            hp.KerasCallback(hp_log_dir + "/validation", hparams)  # log hparams
        ])
    # To track the progression of training, gather a snapshot
    # of the model's metrics at each epoch.
    epochs = history.epoch
    hist = pd.DataFrame(history.history)
    # save model
    model.save(save_dir + session_name + "/" + feature + "/" + r_name)
    test_results = model.evaluate(
        generate_arrays_from_file(feature, tens_path + test_file, hparams[HP_BATCH_SIZE]),
        steps=test_rows[feature] // hparams[HP_BATCH_SIZE])
    return epochs, hist, test_results


if not os.path.exists(save_dir + session_name):
    os.mkdir(save_dir + session_name)

for feat in features:
    if not os.path.exists(save_dir + session_name + "/" + feat):
        os.mkdir(save_dir + session_name + "/" + feat)

for feat in features:
    if not os.path.exists(save_dir + session_name + "/" + feat + "/model_parameters.csv"):
        with open(save_dir + session_name + "/" + feat + "/model_parameters.csv", "w") as f:
            f.write("run_name,learning_rate,num_units,i_weight_decay,r_weight_decay,b_weight_decay,dropout,batch_size,test_loss,test_accuracy,test_precision,test_recall\n")

# if multi gpu machine:
mirrored_strategy = tf.distribute.MirroredStrategy()

for feat in ["schwa"]:
    session_num = 0
    for l_rate in HP_L_RATE.domain.values:
        for n_units in HP_NUM_UNITS.domain.values:
            for batch_size in HP_BATCH_SIZE.domain.values:
                for i_weight_decay in HP_I_W_DECAY.domain.values:
                    for r_weight_decay in HP_R_W_DECAY.domain.values:
                        for b_weight_decay in HP_B_W_DECAY.domain.values:
                            for drpt in HP_DROPOUT.domain.values:
                                hparams = {
                                    HP_L_RATE: l_rate,
                                    HP_NUM_UNITS: n_units,
                                    HP_BATCH_SIZE: batch_size,
                                    HP_I_W_DECAY: i_weight_decay,
                                    HP_R_W_DECAY: r_weight_decay,
                                    HP_B_W_DECAY: b_weight_decay,
                                    HP_DROPOUT: drpt
                                }
                                run_name = "run-" + str(session_num)
                                print("--- Starting trial: " + run_name + " for " + feat)
                                print({h.name: hparams[h] for h in hparams})
                                my_model = create_model(hparams)                    # previously METRICS
                                t0 = time.time()
                                os.mkdir(save_dir + session_name + "/" + feat + "/" + run_name)
                                epochs, hist, test_metrics = train_model(feat, run_name, hparams, my_model)
                                with open(save_dir + session_name + "/" + feat + "/model_parameters.csv", "a") as f:
                                    f.write(",".join([run_name, str(l_rate), str(n_units), str(i_weight_decay), str(r_weight_decay), str(b_weight_decay), str(drpt), str(batch_size)] + [str(i) for i in test_metrics]) + "\n")
                                print(time.time() - t0)
                                session_num += 1


# Establish the model's topography.
# my_model = create_model(learning_rate, METRICS)
# my_model.build(input_shape=(0,(context_size * 2 + 1),39))

# uncomment to get graph plot in tensorboard
#@tf.function
#def traceme(x):
#    return my_model(x)

#writer = tf.summary.create_file_writer(log_dir)
#tf.summary.trace_on(graph=True, profiler=True)
#traceme(tf.zeros((1, (context_size * 2 + 1), 39)))
#with writer.as_default():
#    tf.summary.trace_export(name="model_trace", step=0, profiler_outdir=log_dir)

# Train the model on the normalized training set.
# t0 = time.time()
# epochs, hist = train_model(my_model)
# print(time.time() - t0)
#plot_model(my_model, to_file='/Users/tim/Desktop/DBLSTM.png', show_shapes=True, show_layer_names=False)

# my_model.save(tens_path + "keras_models/")

# Plot a graph of the metric vs. epochs.
#list_of_metrics_to_plot = ['loss', "val_loss"]
#plot_curve(epochs, hist, list_of_metrics_to_plot)
