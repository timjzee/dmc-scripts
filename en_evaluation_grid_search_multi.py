import sys
import subprocess
import multiprocessing

home_dir = "/Volumes/timzee/" if sys.platform == "darwin" else "/home/timzee/"

network_type = ["FFNN", "BLSTM"]    # ["FFNN", "BLSTM"]
context_frames = ["5", "15"]               # [5, 15]; need to train models with 5 context still
model_feature_combos = [(n, c) for n in network_type for c in context_frames]

for nt in network_type:
    for cf in context_frames:
        print("Predicting {} {}".format(nt, cf))
        subprocess.call(["python", home_dir + "GitHub/dmc-scripts/predictAF_en_keras_gs.py", nt, cf])


def gridSearch(data_combos=model_feature_combos, kal_diff_weight=["0.5", "1"], apply_penalty=["1"], n_prec_values=["0", "10"], n_subs_values=["0", "10"], n_smooths=["1", "2", "3"], take_sqrt=["0"], threshold_schwa=["0.2", "0.5", "0.8"], threshold_n=["0.2", "0.5", "0.8"], threshold_N=["0.2", "0.5", "0.8"], subtract_n=["0", "1"]):
    schwa_c_l = []
    n_c_l = []
    N_c_l = []
    schwa_n_l = []
    n_n_l = []
    N_n_l = []
    kdw_l = []
    ap_l = []
    npv_l = []
    nsv_l = []
    ns_l = []
    ts_l = []
    th_s_l = []
    th_n_l = []
    th_N_l = []
    sub_n_l = []
    for schwa_data in data_combos:
        for n_data in data_combos:
            for N_data in data_combos:
                for kdw in kal_diff_weight:
                    for ap in apply_penalty:
                        for npv in n_prec_values:
                            for nsv in n_subs_values:
                                for ns in n_smooths:
                                    for ts in take_sqrt:
                                        for th_s in threshold_schwa:
                                            for th_n in threshold_n:
                                                for th_N in threshold_N:
                                                    for sub_n in subtract_n:
                                                        schwa_c_l.append(schwa_data[1])
                                                        n_c_l.append(n_data[1])
                                                        N_c_l.append(N_data[1])
                                                        schwa_n_l.append(schwa_data[0])
                                                        n_n_l.append(n_data[0])
                                                        N_n_l.append(N_data[0])
                                                        kdw_l.append(kdw)
                                                        ap_l.append(ap)
                                                        npv_l.append(npv)
                                                        nsv_l.append(nsv)
                                                        ns_l.append(ns)
                                                        ts_l.append(ts)
                                                        th_s_l.append(th_s)
                                                        th_n_l.append(th_n)
                                                        th_N_l.append(th_N)
                                                        sub_n_l.append(sub_n)
    return [*zip(schwa_c_l, n_c_l, N_c_l, schwa_n_l, n_n_l, N_n_l, kdw_l, ap_l, npv_l, nsv_l, ns_l, ts_l, th_s_l, th_n_l, th_N_l, sub_n_l)]


def callPraatScript(schwa_context, n_context, N_context, schwa_network, n_network, N_network, kdw, ap, npv, nsv, ns, ts, th_s, th_n, th_N, sub_n):
    subprocess.call(["praat_nogui", "--run", home_dir + "GitHub/dmc-scripts/get_en_boundaries_evaluation_gs.praat", ap, kdw, ns, ts, npv, nsv, schwa_context, n_context, N_context, schwa_network, n_network, N_network, th_s, th_n, th_N, sub_n, "0"])


tasks = gridSearch()
p = multiprocessing.Pool(60)
p.starmap(callPraatScript, iterable=tasks)
