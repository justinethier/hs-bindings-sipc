# A simple makefile to drive everything
all: configure build install
configure:
	cabal configure --prefix=$(HOME) --user
build:
	cabal build
dist:
	cabal install && cabal sdist
clean:
	rm -rf *.o *.hi *.chi Sipc.chs.h C2HS.hs Sipc.hs a.out dist
	find . -type f -name "*.hi" -exec rm -f {} \;
	find . -type f -name "*.o" -exec rm -f {} \;
