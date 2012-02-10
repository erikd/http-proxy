TARGETS = proxy request-rewrite-proxy proxy-test streaming-test connect-test \
			warp-tls-test http-to-https-test

GHC = ghc -Wall -Werror -fwarn-missing-signatures -fwarn-tabs -i:src

LIBSRC = $(shell find Network -name \*.hs)
TESTSRC = $(shell find test -name \*.hs)

all : $(TARGETS)

clean :
	find . -name \*.o -o -name \*.hi -exec rm -f {} \;
	rm -rf dist $(TARGETS)

check : $(TARGETS)
	./proxy-test
	./connect-test
	# The following can be removed later.
	./warp-tls-test
	./streaming-test
	./http-to-https-test

#-------------------------------------------------------------------------------

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@

request-rewrite-proxy : example/request-rewrite-proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@

proxy-test : test/proxy-test.hs test/TestServer.hs $(LIBSRC)
	$(GHC) --make -i:test $< -o $@

streaming-test : test/streaming-test.hs $(TESTSRC) $(LIBSRC)
	$(GHC) --make -i:test $< -o $@

connect-test : test/connect-test.hs $(TESTSRC) $(LIBSRC)
	$(GHC) --make -i:test $< -o $@

warp-tls-test : test/warp-tls-test.hs $(TESTSRC) $(LIBSRC)
	$(GHC) --make -i:test $< -o $@

http-to-https-test : test/http-to-https-test.hs $(TESTSRC) $(LIBSRC)
	$(GHC) --make -i:test $< -o $@

