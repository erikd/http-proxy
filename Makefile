TARGETS = proxy request-rewrite-proxy

GHC = ghc -Wall -fwarn-missing-signatures -fwarn-tabs

LIBSRC := $(shell find Network -name \*.hs)
TESTSRC := $(shell find Test -name \*.hs)

all : $(TARGETS)

clean :
	find . -name \*.o -exec rm -f {} \;
	find . -name \*.hi -exec rm -f {} \;
	rm -rf dist $(TARGETS)

check :
	runghc -Wall Test/testsuite.hs

#-------------------------------------------------------------------------------

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@

request-rewrite-proxy : example/request-rewrite-proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@
