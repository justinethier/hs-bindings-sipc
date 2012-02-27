# A simple makefile to drive everything
dist:
#	runhaskell Setup.hs configure --prefix=$(HOME) --user && runhaskell Setup.hs build && runhaskell Setup.hs install && runhaskell Setup.hs sdist
	cabal configure --prefix=$(HOME) --user && cabal build && cabal install && cabal sdist
#sipc: Sipc.chs
#	c2hs Sipc.chs
#	ghc Sipc.hs -lsipc
#ghci: Sipc.hs
#	ghci Sipc.hs -lsipc
clean:
	rm -rf *.o *.hi *.chi Sipc.chs.h C2HS.hs Sipc.hs a.out dist
	find . -type f -name "*.hi" -exec rm -f {} \;
	find . -type f -name "*.o" -exec rm -f {} \;
