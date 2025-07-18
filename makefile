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


.PHONY: build
build:
	stack build


.PHONY: docs
docs:
	stack haddock --haddock-for-hackage


.PHONY: release
release: docs
	stack upload .
	stack upload --documentation .


.PHONY: clean
clean:
	stack clean --full
