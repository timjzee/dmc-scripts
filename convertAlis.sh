#!/usr/bin/env bash

if [[ "$OSTYPE" == "darwin"* ]]; then
    t_path=/Volumes/timzee/GitHub/dmc-scripts/
else
    t_path=/home/timzee/GitHub/dmc-scripts/
fi

DIR=`pwd`
FILES="$DIR/*"

for f in $FILES
do
  echo "Processing $f file..."
  cat $f | python2 "{$t_path}ali_to_textgrid.py" "./"
done
