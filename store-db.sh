#!/bin/bash -x

copytoram (){
    rsync -rL --exclude="*.qsub" --exclude="*.pddl" --exclude="*.1" $1 /run/shm/$1
}

main (){
    trap "rm -r /run/shm/$1" RETURN SIGINT SIGTERM
    copytoram $1
    find -L /run/shm/$1 -name "*.out" | xargs ./store.bin
}


for d in */
do
    main $d
done
