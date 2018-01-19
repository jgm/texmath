quick:
	stack install --flag 'texmath:executable' --test

test:
	stack test | tee testlog

.PHONY: quick test
