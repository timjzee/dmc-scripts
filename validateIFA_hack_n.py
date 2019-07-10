import sys
import subprocess
import json
import re
import os
# import glob
# import random

tens_path = "/Volumes/tensusers/timzee/cgn/n_tests/" if sys.platform == "darwin" else "/vol/tensusers/timzee/cgn/n_tests/"
ifa_path = "/Volumes/tensusers/timzee/IFAcorpus/" if sys.platform == "darwin" else "/vol/tensusers/timzee/IFAcorpus/"

component = "a"

input_folder = "validation/" + component + "/"

file_names = os.listdir(tens_path + input_folder)
sentences = []
for fn in file_names:
    tiers = os.listdir(tens_path + input_folder + fn + "/")
    for tier in tiers:
        files = os.listdir(tens_path + input_folder + fn + "/" + tier + "/")
        files = [file.split("_")[0] for file in files if "aspex" in file]
        # only use chunks for which 3 segmentations exist (some failed alignments in comp-a)
        dupes = set([file for file in files if files.count(file) > 2])
        for dupe in dupes:
            if re.search(r"[0-9]+\.[0-9]+-[0-9]+\.[0-9]+", dupe):
                sentences.append(fn + "/" + tier + "/" + dupe + "_")

# sentences = ["fn001601/1/1.164-3.706_"]

#line_list = []

with open(tens_path + "validation_data_cgn-kaldi-kaldi-n_" + component + ".csv", "w") as f:
    header = ["sentence", "cgn_labels", "kaldi_n_labels", "kaldi_labels", "cgn_kaldi_n_diff", "cgn_kaldi_diff", "kaldi_n_kaldi_diff\n"]
    f.write(",".join(header))
    for sentence in sentences:
        print("Working on " + sentence)
        max_diff = 3
        while max_diff > 2:
            output = subprocess.check_output(["perl", "-I", ifa_path + "SLcorpus/scripts", ifa_path + "SLcorpus/scripts/ValidateSegmentation.pl", tens_path + input_folder + sentence + "*"])
            output = output[:-2] + "\n]"
            comparisons = json.loads(output)
            if len(comparisons) == 3:
                diff_lens = {}
                for num, i in enumerate(comparisons, 1):
                    diff_lens[num] = len(i[4]["AbsList"])
                max_diff = max([max(diff_lens[1], diff_lens[2]) / min(diff_lens[1], diff_lens[2]), max(diff_lens[1], diff_lens[3]) / min(diff_lens[1], diff_lens[3]), max(diff_lens[2], diff_lens[3]) / min(diff_lens[2], diff_lens[3])])
            else:
                max_diff = 0
        comp_dict = {}
        for c in comparisons:
            cur_path, ref_path, cur_labs, ref_labs, align_dict = c
            sentence2, cur_name = re.search(r"[0-9.A-Z_-]+(?=\..*)", cur_path).group().split("_")
            ref_name = re.search(r"[0-9.A-Z_-]+(?=\..*)", ref_path).group().split("_")[1]
            cur_name_full = "tim" if cur_name == "KA2" else "kaldi" if cur_name == "KA" else "ifa"
            ref_name_full = "tim" if ref_name == "KA2" else "kaldi" if ref_name == "KA" else "ifa"
            if cur_name_full == "ifa" and ref_name_full == "tim":
                comp_dict["ifa_tim"] = {"ifa_labs": cur_labs, "tim_labs": ref_labs, "diff": align_dict["AbsList"]}
            elif cur_name_full == "tim" and ref_name_full == "ifa":
                comp_dict["ifa_tim"] = {"ifa_labs": ref_labs, "tim_labs": cur_labs, "diff": align_dict["AbsList"]}
            elif cur_name_full == "ifa" and ref_name_full == "kaldi":
                comp_dict["ifa_kaldi"] = {"ifa_labs": cur_labs, "kaldi_labs": ref_labs, "diff": align_dict["AbsList"]}
            elif cur_name_full == "kaldi" and ref_name_full == "ifa":
                comp_dict["ifa_kaldi"] = {"ifa_labs": ref_labs, "kaldi_labs": cur_labs, "diff": align_dict["AbsList"]}
            elif cur_name_full == "tim" and ref_name_full == "kaldi":
                comp_dict["tim_kaldi"] = {"kaldi_labs": ref_labs, "tim_labs": cur_labs, "diff": align_dict["AbsList"]}
            else:
                comp_dict["tim_kaldi"] = {"kaldi_labs": cur_labs, "tim_labs": ref_labs, "diff": align_dict["AbsList"]}
        if len(comp_dict) == 1:
            diff_index = 0
            for index, ifa_lab in enumerate(comp_dict["ifa_kaldi"]["ifa_labs"], 0):
                line = {"ifa_label": "", "tim_label": "", "kaldi_label": ""}
                line["tim_label"] = "NA"
                line["ifa_label"] = ifa_lab.split(" ")[-1] if "|" not in ifa_lab else "NA"
                kaldi_lab = comp_dict["ifa_kaldi"]["kaldi_labs"][index]
                line["kaldi_label"] = kaldi_lab.split(" ")[-1] if "|" not in kaldi_lab else "NA"
                line["ifa_tim_diff"] = "NA"
                line["tim_kaldi_diff"] = "NA"
                if line["ifa_label"] != "NA" and line["kaldi_label"] != "NA":
                    line["ifa_kaldi_diff"] = comp_dict["ifa_kaldi"]["diff"][diff_index]
                    diff_index += 1
                else:
                    line["ifa_kaldi_diff"] = "NA"
#                line_list.append(",".join([sentence, line["ifa_label"], line["tim_label"], line["kaldi_label"], line["ifa_tim_diff"], str(line["ifa_kaldi_diff"]), line["tim_kaldi_diff"]]) + "\n")
                f.write(",".join([sentence, line["ifa_label"], line["tim_label"], line["kaldi_label"], line["ifa_tim_diff"], str(line["ifa_kaldi_diff"]), line["tim_kaldi_diff"]]) + "\n")
        else:
            num_ifa_tim = len(comp_dict["ifa_tim"]["ifa_labs"])
            num_tim_kaldi = len(comp_dict["tim_kaldi"]["tim_labs"])
            num_ifa_kaldi = len(comp_dict["ifa_kaldi"]["ifa_labs"])
            index = {"ifa_tim": 0, "tim_kaldi": 0, "ifa_kaldi": 0}
            diff_index = {"ifa_tim": 0, "ifa_kaldi": 0, "tim_kaldi": 0}
            while (index["ifa_tim"] < num_ifa_tim) and (index["tim_kaldi"] < num_tim_kaldi) and (index["ifa_kaldi"] < num_ifa_kaldi):
    #            print(line_list)
                line = {"ifa_label": "", "tim_label": "", "kaldi_label": ""}
                if "|" in comp_dict["ifa_tim"]["ifa_labs"][index["ifa_tim"]]:
                    if "|" in comp_dict["tim_kaldi"]["tim_labs"][index["tim_kaldi"]]:
                        line["ifa_label"] = "NA"
                        line["tim_label"] = "NA"
                        line["kaldi_label"] = comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]].split(" ")[-1]
                        index["tim_kaldi"] += 1
                        index["ifa_kaldi"] += 1
                    elif "|" in comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]]:
                        line["ifa_label"] = "NA"
                        line["kaldi_label"] = "NA"
                        line["tim_label"] = comp_dict["tim_kaldi"]["tim_labs"][index["tim_kaldi"]].split(" ")[-1]
                        index["ifa_tim"] += 1
                        index["tim_kaldi"] += 1
                    else:  # ">", "<" or "=" in comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]]
                        line["ifa_label"] = "NA"
                        line["tim_label"] = comp_dict["tim_kaldi"]["tim_labs"][index["tim_kaldi"]].split(" ")[-1]
                        line["kaldi_label"] = comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]].split(" ")[-1]
                        index["ifa_tim"] += 1
                        index["tim_kaldi"] += 1
                        index["ifa_kaldi"] += 1
                elif "|" in comp_dict["ifa_tim"]["tim_labs"][index["ifa_tim"]]:
                    if "|" in comp_dict["tim_kaldi"]["tim_labs"][index["tim_kaldi"]]:
                        if "|" in comp_dict["ifa_kaldi"]["kaldi_labs"][index["ifa_kaldi"]]:
                            line["tim_label"] = "NA"
                            line["kaldi_label"] = "NA"
                            line["ifa_label"] = comp_dict["ifa_kaldi"]["ifa_labs"][index["ifa_kaldi"]].split(" ")[-1]
                            index["ifa_tim"] += 1
                            index["ifa_kaldi"] += 1
                        elif "|" in comp_dict["ifa_kaldi"]["ifa_labs"][index["ifa_kaldi"]]:
                            line["tim_label"] = "NA"
                            line["ifa_label"] = "NA"
                            line["kaldi_label"] = comp_dict["ifa_kaldi"]["kaldi_labs"][index["ifa_kaldi"]].split(" ")[-1]
                            index["tim_kaldi"] += 1
                            index["ifa_kaldi"] += 1
                        else:
                            line["tim_label"] = "NA"
                            line["ifa_label"] = comp_dict["ifa_tim"]["ifa_labs"][index["ifa_tim"]].split(" ")[-1]
                            line["kaldi_label"] = comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]].split(" ")[-1]
                            index["ifa_tim"] += 1
                            index["tim_kaldi"] += 1
                            index["ifa_kaldi"] += 1
                    elif "|" in comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]]:
                        line["tim_label"] = "NA"
                        line["kaldi_label"] = "NA"
                        line["ifa_label"] = comp_dict["ifa_tim"]["ifa_labs"][index["ifa_tim"]].split(" ")[-1]
                        index["ifa_tim"] += 1
                        index["ifa_kaldi"] += 1
                    else:
                        line["tim_label"] = "NA"
                        line["kaldi_label"] = "NA"
                        line["ifa_label"] = comp_dict["ifa_tim"]["ifa_labs"][index["ifa_tim"]].split(" ")[-1]
                        index["ifa_tim"] += 1
                        index["ifa_kaldi"] += 1
                else:  # ">", "<" or "=" in comp_dict["ifa_tim"]["ifa_labs"][index["ifa_tim"]]
                    if "|" in comp_dict["tim_kaldi"]["tim_labs"][index["tim_kaldi"]]:
                        line["ifa_label"] = "NA"
                        line["tim_label"] = "NA"
                        line["kaldi_label"] = comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]].split(" ")[-1]
                        index["tim_kaldi"] += 1
                        index["ifa_kaldi"] += 1
                    elif "|" in comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]]:
                        line["kaldi_label"] = "NA"
                        line["tim_label"] = comp_dict["ifa_tim"]["tim_labs"][index["ifa_tim"]].split(" ")[-1]
                        line["ifa_label"] = comp_dict["ifa_tim"]["ifa_labs"][index["ifa_tim"]].split(" ")[-1]
                        index["ifa_tim"] += 1
                        index["tim_kaldi"] += 1
                        index["ifa_kaldi"] += 1
                    else:
                        line["ifa_label"] = comp_dict["ifa_tim"]["ifa_labs"][index["ifa_tim"]].split(" ")[-1]
                        line["tim_label"] = comp_dict["ifa_tim"]["tim_labs"][index["ifa_tim"]].split(" ")[-1]
                        line["kaldi_label"] = comp_dict["tim_kaldi"]["kaldi_labs"][index["tim_kaldi"]].split(" ")[-1]
                        index["ifa_tim"] += 1
                        index["tim_kaldi"] += 1
                        index["ifa_kaldi"] += 1
                if line["ifa_label"] != "NA" and line["tim_label"] != "NA":
                    if diff_index["ifa_tim"] > len(comp_dict["ifa_tim"]["diff"]) - 1:
                        print("IFA-TIM INDEX OUT OF RANGE")
                        line["ifa_tim_diff"] = "NA"
                    else:
                        line["ifa_tim_diff"] = comp_dict["ifa_tim"]["diff"][diff_index["ifa_tim"]]
                    diff_index["ifa_tim"] += 1
                else:
                    line["ifa_tim_diff"] = "NA"
                if line["ifa_label"] != "NA" and line["kaldi_label"] != "NA":
                    if diff_index["ifa_kaldi"] > len(comp_dict["ifa_kaldi"]["diff"]) - 1:
                        print("IFA-KALDI INDEX OUT OF RANGE")
                        line["ifa_kaldi_diff"] = "NA"
                    else:
                        line["ifa_kaldi_diff"] = comp_dict["ifa_kaldi"]["diff"][diff_index["ifa_kaldi"]]
                    diff_index["ifa_kaldi"] += 1
                else:
                    line["ifa_kaldi_diff"] = "NA"
                if line["tim_label"] != "NA" and line["kaldi_label"] != "NA":
                    if diff_index["tim_kaldi"] > len(comp_dict["tim_kaldi"]["diff"]) - 1:
                        print("TIM-KALDI INDEX OUT OF RANGE")
                        line["tim_kaldi_diff"] = "NA"
                    else:
                        line["tim_kaldi_diff"] = comp_dict["tim_kaldi"]["diff"][diff_index["tim_kaldi"]]
                    diff_index["tim_kaldi"] += 1
                else:
                    line["tim_kaldi_diff"] = "NA"
                #line_list.append(",".join([sentence, line["ifa_label"], line["tim_label"], line["kaldi_label"], str(line["ifa_tim_diff"]), str(line["ifa_kaldi_diff"]), str(line["tim_kaldi_diff"])]) + "\n")
                f.write(",".join([sentence, line["ifa_label"], line["tim_label"], line["kaldi_label"], str(line["ifa_tim_diff"]), str(line["ifa_kaldi_diff"]), str(line["tim_kaldi_diff"])]) + "\n")


#header = ["sentence", "cgn_labels", "kaldi_n_labels", "kaldi_labels", "cgn_kaldi_n_diff", "cgn_kaldi_diff", "kaldi_n_kaldi_diff\n"]

#with open(tens_path + "validation_data_cgn-kaldi-kaldi-n_" + component + ".csv", "w") as f:
#    f.write(",".join(header))
#    for l in line_list:
#        f.write(l)
