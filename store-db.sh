#!/bin/bash -x

restore (){
    mv -fv db.sqlite.backup db.sqlite
}

set -o errexit
[ -f db.sqlite ] && cp -vb db.sqlite db.sqlite.backup
trap "restore" ERR

dir=/run/shm/$(dirname $(readlink -ef $0))/
mkdir -p $dir

[ -f db.sqlite ] && cp db.sqlite $dir/

trap "mv -vb $dir/db.sqlite db.sqlite" EXIT

ln -sf $dir/db.sqlite .

copy_to_ram (){
    rsync -rL --exclude="*.qsub" --exclude="*.pddl" --exclude="*.1" $1 /run/shm/$1
}
copy_from_ram (){
    rsync -rL --exclude="*.err" --exclude="*.out" /run/shm/$1 $1
}

main (){
    trap "copy_from_ram $1" RETURN
    copy_to_ram $1
    find -L /run/shm/$1 -name "*.out" | xargs ./store.bin
}

for d in */
do
    main $d || break
done
