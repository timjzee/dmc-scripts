import tensorflow as tf
# from tensorflow.keras.utils import plot_model
# from tensorboard.plugins.hparams import api as hp
import sys
import numpy as np
import os


os.nice(19)


tens_path = "/Volumes/tensusers/timzee/af_classification/" if sys.platform == "darwin" else "/vol/tensusers/timzee/af_classification/"
# log_dir = tens_path + "tb_log/"
save_dir = tens_path + "keras_models/"
session_name = "s_5c_16k"
test_file = "Bootstrap_s_large_16k_test.csv"

context_size = 5
# mfcc_length = 429
test_rows = 3681659      # 5c-16k: 3681659; 5c-8k: 2289811; 15c-16k: 3449696; 15c-8k: 2135399


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
                    X = X[:, :-8]
                    #             row/batch size, column/time steps, features
                    X_3d = X.reshape((X.shape[0], (context_size * 2 + 1), 13 * 3))
                    y = np.array(targets, dtype='float32')
                    yield (X_3d, y)
                    inputs = []
                    targets = []
                    batchcount = 0


with open(save_dir + session_name + "/test_accuracies.csv", "a") as g:
    with open(save_dir + session_name + "/model_parameters.csv", "r") as f:
        for line_n, line in enumerate(f, 1):
            if line_n > 14:
                run_name = line[:-1].split(",")[0]
                batch_size = int(line[:-1].split(",")[-1])
                print(run_name)
                my_model = tf.keras.models.load_model(save_dir + session_name + "/" + run_name)
                test_results = my_model.evaluate(
                    generate_arrays_from_file(tens_path + test_file, batch_size),
                    steps=test_rows // batch_size
                )
                g.write(run_name + "," + ",".join([str(i) for i in test_results]) + "\n")
