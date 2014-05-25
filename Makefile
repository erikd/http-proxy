TARGETS = proxy request-rewrite-proxy proxy-test

GHC = ghc -Wall -Werror -fwarn-missing-signatures -fwarn-tabs

LIBSRC := $(shell find Network -name \*.hs)
TESTSRC := $(shell find test -name \*.hs)

all : $(TARGETS)

clean :
	find . -name \*.o -o -name \*.hi -exec rm -f {} \;
	rm -rf dist $(TARGETS)

check : $(TARGETS)
	./proxy-test

#-------------------------------------------------------------------------------

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@

request-rewrite-proxy : example/request-rewrite-proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@

proxy-test : test/proxy-test.hs test/TestServer.hs $(TESTSRC) $(LIBSRC)
	$(GHC) --make -i:test $< -o $@
