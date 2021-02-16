#!/usr/bin/env bash

while read p; do
  fname=`echo "$p" | awk -F ',' '{print $1}'`
  tname=`echo "$p" | awk -F ',' '{print $2}'`
  mv /vol/tensusers/timzee/KALDI_FA_out/v2_comp-b/$fname /vol/tensusers/timzee/KALDI_FA_out/v2_comp-b/$tname
  echo "Renamed $fname to $tname"
done < /vol/tensusers/timzee/cgn/replace_tiers.txt

