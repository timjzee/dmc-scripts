import sys
import subprocess

home_dir = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

network_type = ["FFNN", "BLSTM"]    # ["FFNN", "BLSTM"]
context_frames = ["5", "15"]               # [5, 15]; need to train models with 5 context still
kal_diff_weight = ["0", "0.5", "1"]       # [0, 0.5, 1]
apply_penalty = ["0", "1"]              # [0, 1]
n_prec_values = ["0", "10"]             # [0, 10]
n_subs_values = ["0", "10"]             # [0, 10]
n_smooths = ["0", "1", "2", "3", "4"]         # [0, 1, 2, 3, 4]
take_sqrt = ["0", "1"]                  # [0, 1]

for nt in network_type:
    for cf in context_frames:
        print("Predicting {} {}".format(nt, cf))
        subprocess.call(["python", home_dir + "GitHub/dmc-scripts/predictAF_s_keras_gs.py", nt, cf])
        for kdw in kal_diff_weight:
            for ap in apply_penalty:
                for npv in n_prec_values:
                    for nsv in n_subs_values:
                        for ns in n_smooths:
                            for ts in take_sqrt:
                                subprocess.call(["praat_nogui", "--run", home_dir + "GitHub/dmc-scripts/get_s_boundaries_evaluation_gs.praat", ap, kdw, ns, ts, npv, nsv, cf, nt])
