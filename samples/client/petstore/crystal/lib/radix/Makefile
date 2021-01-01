CRYSTAL ?= crystal

profile ?= ## Display profiling information after specs execution
verbose ?= ## Run specs in verbose mode

SPEC_FLAGS := $(if $(profile),--profile )$(if $(verbose),--verbose )

.PHONY: default autospec spec

default: spec

# `autospec` task uses `watchexec` external dependency:
# https://github.com/mattgreen/watchexec
autospec:
	watchexec --exts cr --watch spec --watch src --clear $(CRYSTAL) spec $(SPEC_FLAGS)

spec:
	$(CRYSTAL) spec $(SPEC_FLAGS)