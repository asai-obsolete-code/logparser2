
$(info $(shell git pull))

.PHONY: pull db ramdisk
all: store.bin

pull:
	-git pull --rebase

%.bin : %.ros $(wildcard *.lisp)
	ros dump executable $<
	mv $(basename $@) $@

db: db.sqlite

db.sqlite: store.bin
	find -L -name "*.out" | xargs ./store.bin 

clean:
	-rm *.bin *.sqlite *~

benchmark: store.bin
	-rm db.sqlite
	@echo first time
	time bash -c 'find -L -name "*.out" | xargs ./store.bin'
	@echo re-insertion
	time bash -c 'find -L -name "*.out" | xargs ./store.bin'

ramdisk:
	sudo mkdir -p /ramdisk
	sudo mount -t ramfs none /ramdisk
	sudo chmod 777 /ramdisk

clean-ramdisk:
	-sudo umount /ramdisk
	-sudo rmdir  /ramdisk
