TARGETS = proxy

GHC = ghc -Wall -fwarn-missing-signatures -i:src

LIBSRC = $(shell find Network -name \*.hs)

all : $(TARGETS)

clean :
	find . -name \*.o -o -name \*.hi -exec rm -f {} \;
	rm -f $(TARGETS)

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@
