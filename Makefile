TARGETS = proxy request-rewrite-proxy testsuite

GHCFLAGS = -Wall -fwarn-tabs
GHC := $(shell if test -f cabal.sandbox.config ; then echo "cabal exec -- ghc $(GHCFLAGS)" ; else echo "ghc $(GHCFLAGS)" ; fi)

HSRC := $(shell find Network Test -name \*.hs)

all : $(TARGETS)

clean :
	find Network Test example -name \*.o -exec rm -f {} \;
	find Network Test example -name \*.hi -exec rm -f {} \;
	rm -rf $(TARGETS)

check : testsuite
	./testsuite

init : cabal.sandbox.config

cabal.sandbox.config :
	cabal sandbox init
	cabal install --dependencies-only

#-------------------------------------------------------------------------------

testsuite : $(HSRC)
	$(GHC) -with-rtsopts="-M512m" -Wall -O2 -threaded Test/testsuite.hs -o $@

proxy : example/proxy.hs $(HSRC)
	$(GHC) --make $< -o $@

request-rewrite-proxy : example/request-rewrite-proxy.hs $(HSRC)
	$(GHC) --make $< -o $@
