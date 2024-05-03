.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test:
	stack test


.PHONY: docs
docs:
	stack haddock --haddock-for-hackage
