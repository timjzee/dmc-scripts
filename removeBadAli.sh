#!/usr/bin/env bash

while read p; do
  rm /vol/tensusers/timzee/KALDI_FA_out/v2_comp-b/$p
  echo "Removed $p"
done < /vol/tensusers/timzee/cgn/w_tiers2.txt

