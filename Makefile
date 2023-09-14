TESTARGS ?=--hide-successes --ansi-tricks=false
CABALOPTS ?=--disable-optimization -fexecutable

test:
	cabal build $(CABALOPTS) --enable-tests
	cabal test $(CABALOPTS) --test-options="$(TESTARGS)"
.PHONY: test

server:
	 cabal install exe:texmath-server -fserver --installdir . --install-method=copy --overwrite-policy=always
	sudo service texmath stop && \
	sudo cp texmath-server /usr/local/bin/texmath-server && \
	sudo service texmath start
.PHONY: server

clean:
	cabal clean
.PHONY: clean
