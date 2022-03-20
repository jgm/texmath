TESTARGS ?= --hide-successes

quick:
	stack install --flag 'texmath:executable' --test --test-arguments='$(TESTARGS)'

test:
	stack test | tee testlog

.PHONY: quick test
