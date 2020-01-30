CSC = csc
LIBFLAGS = -s -d3
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0

scm2wiki: scm2wiki-bin-wrapper.scm scm2wiki.import.so
	$(CSC) scm2wiki-bin-wrapper.scm -b -O3 -o scm2wiki

scm2wiki.so: scm2wiki.scm scm-semantics.import.so semantics2md.import.so
	$(CSC) $(LIBFLAGS) scm2wiki.scm -j scm2wiki

scm2wiki.import.so: scm2wiki.so
	$(CSC) $(IMPORTFLAGS) scm2wiki.import.scm

scm-semantics.so: scm-semantics.scm scm-semantics-impl.import.so
	$(CSC) $(LIBFLAGS) scm-semantics.scm -j scm-semantics

scm-semantics.import.so: scm-semantics.so
	$(CSC) $(IMPORTFLAGS) scm-semantics.import.scm

scm-semantics-impl.so: scm-semantics-impl.scm
	$(CSC) $(LIBFLAGS) scm-semantics-impl.scm -j scm-semantics-impl

scm-semantics-impl.import.so: scm-semantics-impl.so
	$(CSC) $(IMPORTFLAGS) scm-semantics-impl.import.scm

semantics2md.so: semantics2md.scm semantics2md-impl.import.so
	$(CSC) $(LIBFLAGS) semantics2md.scm -j semantics2md

semantics2md.import.so: semantics2md.so
	$(CSC) $(IMPORTFLAGS) semantics2md.import.scm

semantics2md-impl.so: semantics2md-impl.scm
	$(CSC) $(LIBFLAGS) semantics2md-impl.scm -j semantics2md-impl

semantics2md-impl.import.so: semantics2md-impl.so
	$(CSC) $(IMPORTFLAGS) semantics2md-impl.import.scm

run-tests: scm2wiki
	$(info $(shell mkdir -p docs))
	./scm2wiki -i scm2wiki.scm -o docs/scm2wiki.wiki
	./scm2wiki -m -i scm2wiki.scm -o docs/scm2wiki.md

.PHONY: clean

clean:
	-rm scm2wiki *.so
