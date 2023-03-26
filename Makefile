TESTARGS ?=--hide-successes --ansi-tricks=false
CABALOPTS ?=--disable-optimization -fexecutable

test:
	cabal build $(CABALOPTS) --enable-tests
	cabal test $(CABALOPTS) --test-options="$(TESTARGS)"
.PHONY: test

install:
	cabal build $(CABALOPTS)
	rm ~/.cabal/bin/texmath
	cp `cabal list-bin texmath $(CABALOPTS)` ~/.cabal/bin/texmath
.PHONY: install

clean:
	cabal clean
.PHONY: clean
