.PHONY: all default test

default: all

all:
	@cabal build

test:
	@cabal run spec -- -j8
