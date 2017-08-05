.PHONY: test

test ?= chapter-*

test:
	PLTCOLLECTS="$(shell pwd):${PLTCOLLECTS}" raco test --no-run-if-absent ${test}
