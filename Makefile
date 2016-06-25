
.PHONY: pull db
all: store.bin

pull:
	-git pull --rebase

%.bin : %.ros common.lisp
	$(MAKE) pull
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
