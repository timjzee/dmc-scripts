#!/bin/bash

head -n 1 /vol/tensusers/timzee/en_morph_project/nn_all_en_o_kal.csv > /vol/tensusers/timzee/en_morph_project/nn_all_en_o_kal_nl.csv
grep "o/nl/fn" /vol/tensusers/timzee/en_morph_project/nn_all_en_o_kal.csv >> /vol/tensusers/timzee/en_morph_project/nn_all_en_o_kal_nl.csv
