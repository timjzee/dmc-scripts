#!/usr/bin/env bash

if [[ "$OSTYPE" == "darwin"* ]]; then
    t_path=/Volumes/tensusers/timzee/cgn/
else
    t_path=/vol/tensusers/timzee/cgn/
fi

echo "a"

Rscript=$(which Rscript)
Python=$(which python3)

cd "$(dirname "$0")"

echo "b"

$Rscript inspectS.R

echo "c"

$Python makeChunkFile.py

echo "d"

grep -f  "${t_path}chunks2.txt" "${t_path}cgn_index_171218_pron_s.txt" > "${t_path}chunks_KALDI.txt"

echo "e"

$Python addWords.py

echo "f"

# This makes a new chunk file that only contains those chunks that were given a good KALDI alignment
ls -lh "${t_path}KALDI_output/" | grep -v '75B' | awk '{print $9}' | awk 'NF > 0' | sed 's/_/\//' | sed 's/_/\//' | sed 's/_/,/g' | sed 's/.\{4\}$//' > "${t_path}temp.tmp"
head -n 1 "${t_path}chunks_PRAAT.txt" > "${t_path}chunks_PRAAT2.txt"
grep -f "${t_path}temp.tmp" "${t_path}chunks_PRAAT.txt" >> "${t_path}chunks_PRAAT2.txt"
rm "${t_path}temp.tmp"
