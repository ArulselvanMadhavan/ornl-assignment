#!/bin/bash -l
latest_build=$(ls -Art build/ | tail -n 1)
build_dir=build/$latest_build/app/ornl-assignment
output_file=data/runtime.csv

echo "iteration,problem_size,number_of_nodes,execution_time_in_ms" > $output_file
for i in 1 2 3
do
    for np in 1 2 4 6
    do
        for p in 600000 3600000 6000000 12000000 24000000
        do
            echo "Running problem_size=$p on $np nodes"
            start=`date +%s%N`
            cafrun -np $np $build_dir -p:$p
            end=`date +%s%N`
            diff=`expr $end - $start`
            diff=`expr $diff / 1000000`
            echo "$i,$p,$np,$diff" >> $output_file
            echo Execution time was $diff milliseconds.
        done
    done
done
