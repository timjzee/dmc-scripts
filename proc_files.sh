#!/bin/bash

cd /vol/tensusers/timzee/af_classification

cat Bootstrap_en_5c_o.csv | shuf > Bootstrap_en_5c_16k_shuf.csv

num_lines=$(awk 'END { print NR }' Bootstrap_en_5c_16k_shuf.csv)
ten_perc=$((num_lines / 10))
train_lines=$((num_lines - ten_perc * 2))
valid_lines=$((num_lines - ten_perc))
head -n "$train_lines" Bootstrap_en_5c_16k_shuf.csv > Bootstrap_en_5c_16k_train.csv
head -n "$valid_lines" Bootstrap_en_5c_16k_shuf.csv | tail -n "$ten_perc" > Bootstrap_en_5c_16k_valid.csv
cat Bootstrap_en_5c_16k_shuf.csv | tail -n "$ten_perc" > Bootstrap_en_5c_16k_test.csv

train_schwa=$(awk -F ',' '$438 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_train.csv)
train_n=$(awk -F ',' '$439 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_train.csv)
train_nas=$(awk -F ',' '$440 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_train.csv)

valid_schwa=$(awk -F ',' '$438 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_valid.csv)
valid_n=$(awk -F ',' '$439 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_valid.csv)
valid_nas=$(awk -F ',' '$440 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_valid.csv)

test_schwa=$(awk -F ',' '$438 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_test.csv)
test_n=$(awk -F ',' '$439 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_test.csv)
test_nas=$(awk -F ',' '$440 > -0.5 { count++ } END { print count }' Bootstrap_en_5c_16k_test.csv)

echo "[{\"schwa\": $train_schwa, \"nasal\": $train_n, \"nasalization\": $train_nas}, {\"schwa\": $valid_schwa, \"nasal\": $valid_n, \"nasalization\": $valid_nas}, {\"schwa\": $test_schwa, \"nasal\": $test_n, \"nasalization\": $test_nas}]" > training_numbers_5.json

cat Bootstrap_en_15c_o.csv | shuf > Bootstrap_en_15c_16k_shuf.csv

num_lines=$(awk 'END { print NR }' Bootstrap_en_15c_16k_shuf.csv)
ten_perc=$((num_lines / 10))
train_lines=$((num_lines - ten_perc * 2))
valid_lines=$((num_lines - ten_perc))
head -n "$train_lines" Bootstrap_en_15c_16k_shuf.csv > Bootstrap_en_15c_16k_train.csv
head -n "$valid_lines" Bootstrap_en_15c_16k_shuf.csv | tail -n "$ten_perc" > Bootstrap_en_15c_16k_valid.csv
cat Bootstrap_en_15c_16k_shuf.csv | tail -n "$ten_perc" > Bootstrap_en_15c_16k_test.csv

train_schwa=$(awk -F ',' '$438 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_train.csv)
train_n=$(awk -F ',' '$439 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_train.csv)
train_nas=$(awk -F ',' '$440 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_train.csv)

valid_schwa=$(awk -F ',' '$438 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_valid.csv)
valid_n=$(awk -F ',' '$439 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_valid.csv)
valid_nas=$(awk -F ',' '$440 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_valid.csv)

test_schwa=$(awk -F ',' '$438 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_test.csv)
test_n=$(awk -F ',' '$439 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_test.csv)
test_nas=$(awk -F ',' '$440 > -0.5 { count++ } END { print count }' Bootstrap_en_15c_16k_test.csv)

echo "[{\"schwa\": $train_schwa, \"nasal\": $train_n, \"nasalization\": $train_nas}, {\"schwa\": $valid_schwa, \"nasal\": $valid_n, \"nasalization\": $valid_nas}, {\"schwa\": $test_schwa, \"nasal\": $test_n, \"nasalization\": $test_nas}]" > training_numbers_15.json
