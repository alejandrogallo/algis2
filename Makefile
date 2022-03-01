all: doc test
SRC = $(shell find src/ -name '*.hs')
TST = $(shell find tests/ -name '*.hs')

doc:
	cabal haddock  --haddock-hyperlink-source

test:
	cabal test

watch:
	echo $(SRC) $(TST) | tr ' ' '\n' | entr make -j test
