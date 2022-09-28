#!/bin/bash

fileName="ag_fsharp.csv"
touch $fileName
echo "time(sec) memeory(kB)" > $fileName

time=0.0
memory=0.0
for i in $(seq 1 1 10)
do
    output=$(/usr/bin/time -f "%e %M" ag $1 2>&1 >/dev/null)
    outputArray=($output)
    time=$(python3 -c $time+${outputArray[0]})
    memory+=$(python3 -c $memory+${outputArray[1]})
done
time=$(python3 -c $time/10)
memory=$(python3 -c $memory/10)

echo $time
echo $memory

# echo "$(cat $fileName | sed 's/\s/,/')" > $fileName
