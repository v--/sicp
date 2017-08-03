.PHONY: test

test ?= chapter-*

test:
	PLTCOLLECTS="$(shell pwd):${PLTCOLLECTS}" raco test ${test}
