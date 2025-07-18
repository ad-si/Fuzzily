.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: format
format:
	fourmolu --mode inplace $$(fd -e hs)


.PHONY: lint
lint:
	hlint \
		--ignore-glob="archive" \
		--ignore="Redundant do" \
		--ignore="Use list literal" \
		--ignore="Use String" \
		--ignore="Redundant bracket" \
		--ignore="Use camelCase" \
		.


.PHONY: test-unit
test-unit:
	stack test


.PHONY: test
test: lint test-unit


.PHONY: docs
docs:
	stack haddock --haddock-for-hackage
