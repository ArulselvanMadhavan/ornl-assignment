#!/bin/bash -l
latest_build=$(ls -Art build/ | tail -n 1)
build_dir=build/$latest_build/app/ornl-assignment
for np in 1
do
    for p in 60000 600000
    do
        echo "Running problem_size=$p on $np nodes"
        start=`date +%s%N`
        cafrun -np $np $build_dir -p:$p
        end=`date +%s%N`
        diff=`expr $end - $start`
        diff=`expr $diff / 1000000`
        echo Execution time was $diff milliseconds.
    done
done
