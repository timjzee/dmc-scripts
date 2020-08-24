import tensorflow as tf
import pandas as pd
import sys
import numpy as np
from matplotlib import pyplot as plt
import time
import os

os.nice(19)
# os.environ["CUDA_DEVICE_ORDER"] = "PCI_BUS_ID"
# os.environ["CUDA_VISIBLE_DEVICES"] = "0"  # use id from $ nvidia-smi


tens_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"

mfcc_length = 429
corpora = ["cgn-a", "cgn-c", "cgn-d", "cgn-k", "cgn-o", "ifadv", "ecsd", "ifa"]
num_rows = 10000
batch_size = 1000
learning_rate = 0.005
n_epochs = 20
classification_threshold = 0.5

# Establish the metrics the model will measure.
METRICS = [
    tf.keras.metrics.BinaryAccuracy(name='accuracy', threshold=classification_threshold),
    tf.keras.metrics.Precision(name='precision', thresholds=classification_threshold),
    tf.keras.metrics.Recall(name='recall', thresholds=classification_threshold)
]

callbacks = [
    tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=3)
]


def generate_arrays_from_file(path, batchsize):
    """Load data."""
    inputs = []
    targets = []
    batchcount = 0
    while True:
        with open(path) as f:
            for line in f:
                line_list = line[:-1].split(',')
                inputs.append(line_list[:-1])
                targets.append(line_list[-1])
                batchcount += 1
                if batchcount > batchsize:
                    X = np.array(inputs, dtype='float32')
                    y = np.array(targets, dtype='float32')
                    yield (X, y)
                    inputs = []
                    targets = []
                    batchcount = 0


def plot_curve(epochs, hist, list_of_metrics):
    """Plot a curve of one or more classification metrics vs. epoch."""
    # list_of_metrics should be one of the names shown in:
    # https://www.tensorflow.org/tutorials/structured_data/imbalanced_data#define_the_model_and_metrics
    plt.figure()
    plt.xlabel("Epoch")
    plt.ylabel("Value")
    for m in list_of_metrics:
        x = hist[m]
        plt.plot(epochs[1:], x[1:], label=m)
    plt.legend()
    plt.show()


def create_model(my_learning_rate, my_metrics):
    """Create and compile a deep neural net."""
    model = tf.keras.models.Sequential()
    # Define input layer
#    model.add(tf.keras.layers.Dense(units=mfcc_length + len(corpora)))
    # Define the hidden layer.
    model.add(tf.keras.layers.Dense(units=300, activation='relu'))
    # Define dropout layer for regularization
    model.add(tf.keras.layers.Dropout(0.9))
    # Define the output layer.
    model.add(tf.keras.layers.Dense(units=1, activation='sigmoid'))
    # Construct the layers into a model that TensorFlow can execute.
    model.compile(
        optimizer=tf.keras.optimizers.Adam(lr=my_learning_rate),
        loss="binary_crossentropy",
        metrics=my_metrics)
    return model


def train_model(model):
    """Train the model by feeding it data."""
    history = model.fit(
        generate_arrays_from_file(tens_path + "toy_s_train.csv", batch_size),
        steps_per_epoch=num_rows / batch_size, epochs=n_epochs,
        validation_data=generate_arrays_from_file(tens_path + "toy_s_valid.csv", batch_size),
        validation_steps=num_rows / batch_size,
        callbacks=callbacks)
    # To track the progression of training, gather a snapshot
    # of the model's metrics at each epoch.
    epochs = history.epoch
    hist = pd.DataFrame(history.history)
    return epochs, hist


# Establish the model's topography.
my_model = create_model(learning_rate, METRICS)

# Train the model on the normalized training set.
t0 = time.time()
epochs, hist = train_model(my_model)
print(time.time() - t0)

# Plot a graph of the metric vs. epochs.
list_of_metrics_to_plot = ['loss', "val_loss"]
plot_curve(epochs, hist, list_of_metrics_to_plot)
