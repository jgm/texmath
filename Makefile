quick:
	stack install --flag 'texmath:executable' --no-run-tests --test

test:
	stack test | tee testlog

.PHONY: quick test
