#!/usr/bin/env bash

if [[ "$OSTYPE" == "darwin"* ]]; then
    t_dir=/Volumes/timzee/robert/
    dmc_dir=/Volumes/timzee/GitHub/dmc-scripts/
else
    t_dir=/home/timzee/robert/
    dmc_dir=/home/timzee/GitHub/dmc-scripts/
fi

cores=$1

num_lines=`cat "${t_dir}expgrammar_order.txt" | wc -l`
core_lines=$((num_lines / cores))


run_core(){
    echo $core
    if [ $core -eq $cores ]; then
        end_line=$num_lines
        prev_core=$((core - 1))
        core_lines2=$((num_lines - prev_core * core_lines))
    else
        end_line=$((core * core_lines))
        core_lines2=$core_lines
    fi
    echo $end_line
    echo $core_lines2
    head -n $end_line "${t_dir}expgrammar_order.txt" | tail -n $core_lines2 > "${t_dir}grep_file${core}.txt"
    python3 "${dmc_dir}grep-f_robert.py" "${t_dir}grep_file${core}.txt" "${t_dir}taaltest_compleet.txt" > "${t_dir}times${core}.txt"
}

for core in `seq 1 $cores`;
do
    run_core $core &
done

wait

echo "merging outputs"

for core in `seq 1 $cores`;
do
    if [ $core -eq 1 ]; then
        cat "${t_dir}times${core}.txt" > "${t_dir}times.txt"
    else
        cat "${t_dir}times${core}.txt" >> "${t_dir}times.txt"
    fi
    rm "${t_dir}times${core}.txt" "${t_dir}grep_file${core}.txt"
done
