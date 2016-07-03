
$(info $(shell git pull))

.PHONY: pull db ramdisk distclean clean
all: store.bin

pull:
	-git pull --rebase

%.bin : %.ros $(wildcard *.lisp)
	ros dump executable $<
	mv $(basename $@) $@

db: db.sqlite

db.sqlite: store.bin
	./store-db.sh

clean:
	-rm *.sqlite *~
distclean: clean
	-rm *.bin

benchmark: store.bin
	-rm db.sqlite
	@echo first time
	time bash -c 'find -L -name "*.out" | xargs ./store.bin'
	@echo re-insertion
	time bash -c 'find -L -name "*.out" | xargs ./store.bin'


