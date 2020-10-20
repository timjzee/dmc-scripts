#!/usr/bin/env bash

for f in *.awd; do
    echo "$f"
    encoding=`file -i $f | awk -F '=' '{print $2}'`
    if [[ $encoding == "utf-16be" ]]; then
        iconv -f $encoding -t utf-8 $f > tmp.awd
        mv tmp.awd $f
        echo "Changed encoding!"
    fi
done
