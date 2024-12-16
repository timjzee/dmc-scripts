#!/bin/bash

awk -F ',' '{print $23}' /vol/tensusers/timzee/en_morph_project/e-BLSTM-15_n-BLSTM-15_N-BLSTM-5_1_1_0_0_2_0_0.2_0.5_0.5_1.csv | tail -n +2 > spkr.grp
echo "speaker_region" > spkr_reg.txt
echo "speaker_education" > spkr_edu.txt
while read vl; do grep "$vl" /vol/tensusers/timzee/cgn/speakers.txt; done < spkr.grp | awk -F '\t' '{print $20}' >> spkr_edu.txt
while read vl; do grep "$vl" /vol/tensusers/timzee/cgn/speakers.txt; done < spkr.grp | awk -F '\t' '{print $9}' >> spkr_reg.txt
paste -d ',' /vol/tensusers/timzee/en_morph_project/e-BLSTM-15_n-BLSTM-15_N-BLSTM-5_1_1_0_0_2_0_0.2_0.5_0.5_1.csv spkr_reg.txt spkr_edu.txt > output_file3.csv
