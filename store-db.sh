#!/bin/bash -x

copytoram (){
    rsync -rL --exclude="*.qsub" --exclude="*.pddl" --exclude="*.1" $1 /run/shm/$1
}

main (){
    trap "rm -r /run/shm/$1" RETURN
    copytoram $1
    find -L /run/shm/$1 -name "*.out" | xargs ./store.bin
}

trap "rm db.sqlite; mv /run/shm/db.sqlite ." EXIT

ln -s /run/shm/db.sqlite .

for d in */
do
    main $d
done
