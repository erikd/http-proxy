TARGETS = proxy

GHC = ghc -Wall -fwarn-missing-signatures -i:src


all : $(TARGETS)

clean :
	rm -f *.o *.hi $(TARGETS)

proxy : proxy.hs Timeout.hs
	$(GHC) --make $< -o $@
