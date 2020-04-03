CSC = csc
LIBFLAGS = -s -d3
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0

scm2wiki: scm2wiki.scm scm-semantics.import.so semantics2md.import.so
	$(CSC) scm2wiki.scm -b -O3 -o scm2wiki

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

.PHONY: run-tests
run-tests: scm-semantics.import.so semantics2md.import.so
	cp -t ./ tests/run.scm && csi run.scm -e
	-rm run.scm

.PHONY: clean
clean:
	-rm scm2wiki *.so
