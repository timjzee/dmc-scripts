#!/usr/bin/env bash

for f in *.ort; do
    echo "$f"
    encoding=`file -i $f | awk -F '=' '{print $2}'`
    if [[ $encoding == "utf-16be" ]]; then
        iconv -f $encoding -t utf-8 $f > tmp.ort
        mv tmp.ort $f
        echo "Changed encoding!"
    fi
done
