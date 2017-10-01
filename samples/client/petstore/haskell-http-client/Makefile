STACKCMD := stack --install-ghc
.PHONY: all clean build test test-integration build-example build-integration 
all: clean build test build-example build-integration 
clean: ; (rm -Rf ./.stack-work ./example-app/.stack-work ./tests-integration/.stack-work);
build: ; ($(STACKCMD) haddock);
test: ; ($(STACKCMD) test);
build-example: build ; (cd ./example-app; $(STACKCMD) build);
# a test-only project may exit with ExitFailure, despite building successfully
build-integration: build ; (cd ./tests-integration; $(STACKCMD) build --no-run-tests); 
test-integration: build-integration ; (cd ./tests-integration; $(STACKCMD) test);
