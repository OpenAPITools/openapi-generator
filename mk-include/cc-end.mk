# Include this file last
.PHONY: init-ci
## Target for CI to setup environment
init-ci: $(INIT_CI_TARGETS)

RELEASE_PRECOMMIT ?=
RELEASE_POSTCOMMIT ?= $(RELEASE_TARGETS) # set to RELEASE_TARGETS for backwards compatibility
RELEASE_TARGETS := $(RELEASE_PRECOMMIT) get-release-image commit-release tag-release $(RELEASE_POSTCOMMIT)

.PHONY: release
## Release Project.  See show-args to see what will run
release: $(RELEASE_TARGETS)
ifneq ($(RELEASE_MAKE_TARGETS),)
	. mk-include/bin/vault-setup
	$(MAKE) $(MAKE_ARGS) $(RELEASE_MAKE_TARGETS)
endif

.PHONY: release-ci
## Target for CI to run to release
release-ci:
ifeq ($(CI),true)
ifneq ($(RELEASE_BRANCH),$(_empty))
	$(MAKE) $(MAKE_ARGS) release
else ifeq ($(CI_TEST), true)
	$(MAKE) $(MAKE_ARGS) release
endif
# auto merge if it is a mk-include auto update
ifeq ($(TRIGGER_PR),false)
ifeq ($(UPDATE_MK_INCLUDE_AUTO_MERGE),true)
	$(MAKE) auto-merge
endif
endif
else
	true
endif

.PHONY: pre-release-check
pre-release-check:
	test -f go.sum && git checkout go.sum || true # discard local go.sum modifications
	git diff --exit-code || (echo "ERROR: the repo is not supposed to have local dirty changes prior to releasing" && git status && exit 1)

.PHONY: auto-merge
auto-merge:
ifeq ($(CI),true)
	@$(MAKE) install-github-cli
	$(GH) pr merge $(SEMAPHORE_GIT_PR_BRANCH) -s || $(GH) pr merge $(SEMAPHORE_GIT_BRANCH) -s
endif

.PHONY: build
## Build Project.  See show-args to see what will run
build: $(BUILD_TARGETS)

.PHONY: test
## Test Project.  See show-args to see what will run
test: $(TEST_TARGETS)

.PHONY: clean
## Clean Project.  See show-args to see what will run
clean: $(CLEAN_TARGETS)

.PHONY: epilogue-ci
## Epilogue (post-build steps for CI).  See show-args to see what will run
epilogue-ci:
ifeq ($(CI),true)
	$(MAKE) $(MAKE_ARGS) $(EPILOGUE_TARGETS)
endif

.PHONY: show-args
## Show what common targets will run.
show-args:
	@echo "INIT_CI_TARGETS:      $(INIT_CI_TARGETS)"
	@echo "CLEAN_TARGETS:        $(CLEAN_TARGETS)"
	@echo "BUILD_TARGETS:        $(BUILD_TARGETS)"
	@echo "TEST_TARGETS:         $(TEST_TARGETS)"
	@echo "RELEASE_TARGETS:      $(RELEASE_TARGETS)"
	@echo "RELEASE_MAKE_TARGETS: $(RELEASE_MAKE_TARGETS)"
	@echo "EPILOGUE_TARGETS:     $(EPILOGUE_TARGETS)"
	@echo "CI_BIN:               $(CI_BIN)"
	@echo "BIN_PATH:             $(BIN_PATH)"
	@echo "HOST_OS:              $(HOST_OS)"
	@echo "GIT_ROOT:             $(GIT_ROOT)"

.DEFAULT_GOAL := help
.PHONY: help
## Show this help
help:
	@echo "$$(tput bold)Available rules:$$(tput sgr0)"
	@echo
	@sed -n -e "/^## / { \
		h; \
		s/.*//; \
		:doc" \
		-e "H; \
		n; \
		s/^## //; \
		t doc" \
		-e "s/:.*//; \
		G; \
		s/\\n## /---/; \
		s/\\n/ /g; \
		p; \
	}" ${MAKEFILE_LIST} \
	| LC_ALL='C' sort --ignore-case \
	| awk -F '---' \
		-v ncol=$$(tput cols) \
		-v indent=19 \
		-v col_on="$$(tput setaf 6)" \
		-v col_off="$$(tput sgr0)" \
	'{ \
		printf "%s%*s%s ", col_on, -indent, $$1, col_off; \
		n = split($$2, words, " "); \
		line_length = ncol - indent; \
		for (i = 1; i <= n; i++) { \
			line_length -= length(words[i]) + 1; \
			if (line_length <= 0) { \
				line_length = ncol - indent - length(words[i]) - 1; \
				printf "\n%*s ", -indent, " "; \
			} \
			printf "%s ", words[i]; \
		} \
		printf "\n"; \
	}' \
	| more $(shell test $(shell uname) = Darwin && echo '--no-init --raw-control-chars')
