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
