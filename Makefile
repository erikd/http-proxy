TARGETS = proxy request-rewrite-proxy proxy-test

GHC = ghc -Wall -fwarn-missing-signatures -fwarn-tabs -i:src

LIBSRC = $(shell find Network -name \*.hs)

all : $(TARGETS)

clean :
	find . -name \*.o -o -name \*.hi -exec rm -f {} \;
	rm -rf dist $(TARGETS)

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@

request-rewrite-proxy : example/request-rewrite-proxy.hs
	$(GHC) --make $< -o $@

proxy-test : test/proxy-test.hs test/TestServer.hs
	$(GHC) --make -i:test $< -o $@
