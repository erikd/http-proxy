TARGETS = proxy

GHC = ghc -Wall -fwarn-missing-signatures -fwarn-tabs -i:src

LIBSRC = $(shell find Network -name \*.hs)

all : $(TARGETS)

clean :
	find . -name \*.o -o -name \*.hi -exec rm -f {} \;
	rm -rf dist $(TARGETS)

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@
