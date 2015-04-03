TARGETS = proxy request-rewrite-proxy

GHC = ghc -Wall -fwarn-tabs

LIBSRC := $(shell find Network -name \*.hs)
TESTSRC := $(shell find Test -name \*.hs)

all : $(TARGETS)

clean :
	find . -name \*.o -exec rm -f {} \;
	find . -name \*.hi -exec rm -f {} \;
	rm -rf dist $(TARGETS)

check : testsuite
	./testsuite

#-------------------------------------------------------------------------------

testsuite : $(TESTSRC) $(LIBSRC)
	$(GHC) -with-rtsopts="-M512m" -Wall -O2 -threaded Test/testsuite.hs -o $@

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@

request-rewrite-proxy : example/request-rewrite-proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@
