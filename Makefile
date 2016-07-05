
# $(info $(shell git pull))

.PHONY: pull db ramdisk distclean clean
all: store.bin

pull:
	-git pull --rebase

%.bin : %.ros $(wildcard *.lisp)
	ros dump executable $<
	mv $(basename $@) $@

db: store.bin
	./store-db.sh 2>&1 | tee log

clean:
	-rm *.sqlite *~ *.backup
	-find -name "*.parsed" -delete
distclean: clean
	-rm *.bin

benchmark: store.bin
	-rm db.sqlite
	@echo first time
	time bash -c 'find -L -name "*.out" | xargs ./store.bin'
	@echo re-insertion
	time bash -c 'find -L -name "*.out" | xargs ./store.bin'


