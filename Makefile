TARGETS = proxy

GHC = ghc -Wall -fwarn-missing-signatures -i:src

LIBSRC = $(shell find Network -name \*.hs)

all : $(TARGETS)

clean :
	rm -f *.o *.hi $(TARGETS)

proxy : example/proxy.hs $(LIBSRC)
	$(GHC) --make $< -o $@
