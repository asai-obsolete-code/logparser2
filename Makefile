
# $(info $(shell git pull))

.PHONY: pull db ramdisk distclean clean graph plot-clean addindex
all: store.bin

pull:
	-git pull --rebase

%.bin : %.ros $(wildcard *.lisp)
	ros dump executable $<
	mv $(basename $@) $@

db: store.bin
	./store-db.sh 2>&1 | tee log
	$(MAKE) addindex

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

dropbox = ~/Dropbox/FukunagaLabShare/OngoingWorks/Asai/$(notdir $(CURDIR))/

%.plot: %.ros plot-common.lisp db.sqlite
	ros $<
	touch $@

plot: $(patsubst %.ros,%.plot,$(wildcard plot*.ros))
	mkdir -p $(dropbox)
	rsync -raz --delete evaluation generation expansion $(dropbox)

plot-clean:
	 -rm -r *.plot evaluation* generation* expansion*

test:
	./test.sh

addindex:
	-sqlite3 db.sqlite "create index _fig2 on fig2 (problem,domain_id,heuristics_id,algorithm_id,tag_id)"
	-sqlite3 db.sqlite "create index _fig3 on fig3 (problem,domain_id,heuristics_id,algorithm_id,tag_id)"
