STACKCMD := stack --install-ghc
.PHONY: all clean build test test-integration build-example build-integration 
all: clean build test build-example build-integration 
clean: ; (rm -Rf ./.stack-work ./example-app/.stack-work ./tests-integration/.stack-work);
build: ; ($(STACKCMD) haddock --no-haddock-deps --fast);
test: ; ($(STACKCMD) test --fast);
build-example: build ; (cd ./example-app; $(STACKCMD) build --fast);
# a test-only project may exit with ExitFailure, despite building successfully
build-integration: build ; (cd ./tests-integration; $(STACKCMD) build --no-run-tests --fast); 
test-integration: build-integration ;  (cd ./tests-integration; $(STACKCMD) test --fast);
