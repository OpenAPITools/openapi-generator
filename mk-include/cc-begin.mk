# Set shell to bash
SHELL := /bin/bash

# Use this variable to specify a different make utility (e.g. remake --profile)
MAKE ?= make

# Include this file first
_empty :=
_space := $(_empty) $(empty)

# Master branch
MASTER_BRANCH ?= master

# set default update version to master
MK_INCLUDE_UPDATE_VERSION ?= master

# set default mk-include auto update to false, default mk-include auto merge to true
UPDATE_MK_INCLUDE ?= false
UPDATE_MK_INCLUDE_AUTO_MERGE ?= true

ifeq (true, $(UPDATE_MK_INCLUDE))
INIT_CI_TARGETS += diff-mk-include
endif
RELEASE_TARGETS += $(_empty)
BUILD_TARGETS += $(_empty)
TEST_TARGETS += $(_empty)
CLEAN_TARGETS += $(_empty)

# If this variable is set, release will run $(MAKE) $(RELEASE_MAKE_TARGETS)
RELEASE_MAKE_TARGETS +=

ifeq ($(SEMAPHORE), true)
ifeq ($(SEMAPHORE_PROJECT_ID),)
# The SEMAPHORE_PROJECT_ID variable is only set in sem2 environments
SEMAPHORE_2 := false
else
SEMAPHORE_2 := true
endif
endif

GIT_ROOT ?= $(CURDIR)
ifeq ($(SEMAPHORE_2),true)
# TODO: try to clean up .semaphore/semaphore.yml files.
# export GO111MODULE=on
# export "GOPATH=$(go env GOPATH)"
# export "SEMAPHORE_GIT_DIR=${GOPATH}/src/github.com/confluentinc/${SEMAPHORE_PROJECT_NAME}"
# export "PATH=${GOPATH}/bin:${PATH}:/usr/local/kubebuilder/bin:/usr/local/kubebuilder"
# mkdir -vp "${SEMAPHORE_GIT_DIR}" "${GOPATH}/bin"
# export SEMAPHORE_CACHE_DIR=/home/semaphore
ifeq ($(abspath $(SEMAPHORE_GIT_DIR)),$(SEMAPHORE_GIT_DIR))
GIT_ROOT := $(SEMAPHORE_GIT_DIR)
else
GIT_ROOT := $(HOME)/$(SEMAPHORE_GIT_DIR)
endif
# Place ci-bin inside the project as Semaphore 2 only allows caching resources within the project workspace.
# This needs to be different from $(GO_OUTDIR) so it doesn't get cleaned up by clean-go target.
CI_BIN := $(GIT_ROOT)/ci-bin
else ifeq ($(SEMAPHORE),true)
GIT_ROOT := $(SEMAPHORE_PROJECT_DIR)
CI_BIN := $(SEMAPHORE_CACHE_DIR)/bin
else ifeq ($(BUILDKITE),true)
CI_BIN := /tmp/bin
endif

# Defaults
MK_INCLUDE_BIN ?= ./mk-include/bin

MK_INCLUDE_DATA := mk-include/data

# Where test reports get generated, used by testbreak reporting.
ifeq ($(SEMAPHORE),true)
BUILD_DIR := $(GIT_ROOT)/build
else
BUILD_DIR := /tmp/build
endif
export BUILD_DIR

HOST_OS := $(shell uname | tr A-Z a-z)

ifeq ($(BIN_PATH),)
ifeq ($(CI),true)
BIN_PATH := $(CI_BIN)
else
ifeq ($(HOST_OS),darwin)
BIN_PATH ?= /usr/local/bin
else
ifneq ($(wildcard $(HOME)/.local/bin/.),)
BIN_PATH ?= $(HOME)/.local/bin
else
BIN_PATH ?= $(HOME)/bin
endif
endif
endif
endif

XARGS := xargs
ifeq ($(HOST_OS),linux)
XARGS += --no-run-if-empty
endif

ifeq ($(CI),true)
# downstream things (like cpd CI) assume BIN_PATH exists
$(shell mkdir -p $(BIN_PATH) 2>/dev/null)
PATH := $(BIN_PATH):$(PATH)
export PATH
endif

# Git stuff
BRANCH_NAME ?= $(shell git rev-parse --abbrev-ref HEAD || true)
# Set RELEASE_BRANCH if we're on master or vN.N.x
# special case for ce-kafka: v0.NNNN.x-N.N.N-ce-SNAPSHOT, v0.NNNN.x-N.N.N-N-ce
RELEASE_BRANCH := $(shell echo $(BRANCH_NAME) | grep -E '^($(MASTER_BRANCH)|v[0-9]+\.[0-9]+\.x(-[0-9]+\.[0-9]+\.[0-9](-[0-9])?(-ce)?(-SNAPSHOT)?)?)$$')
# assume the remote name is origin by default
GIT_REMOTE_NAME ?= origin

# Makefile called
MAKEFILE_NAME ?= Makefile
MAKE_ARGS := -f $(MAKEFILE_NAME)

# Determine if we're on a hotfix branch
ifeq ($(RELEASE_BRANCH),$(MASTER_BRANCH))
HOTFIX := false
else
HOTFIX := true
endif

# mock GIT command under CI_TEST environemnt
ifeq ($(CI_TEST),true)
GIT := echo git
else
GIT := git
endif

ifeq ($(CI),true)
_ := $(shell test -d $(CI_BIN) || mkdir -p $(CI_BIN))
export PATH = $(CI_BIN):$(shell printenv PATH)
endif

ifeq (true, $(UPDATE_MK_INCLUDE))
MK_INCLUDE_UPDATE_BRANCH := chore-update-mk-include
endif
# default mk-include git hash location
MK_INCLUDE_GIT_HASH_LOCATION := $(MK_INCLUDE_DATA)/mk-include-git-hash
ifeq ($(MK_INCLUDE_UPDATE_VERSION),master)
MK_INCLUDE_GIT_HASH := $(shell git ls-remote --tags git@github.com:confluentinc/cc-mk-include.git \
| sort -t '/' -k 3 -V | tail -n1 | tr -d " \t\n\r" | sed -E -e "s/refs\/(tags|heads)//")
else
MK_INCLUDE_GIT_HASH := $(shell git ls-remote --tags git@github.com:confluentinc/cc-mk-include.git $(MK_INCLUDE_UPDATE_VERSION)\
| tail -n1 | tr -d " \t\n\r" | sed -E -e "s/refs\/(tags|heads)//")
endif
MK_INCLUDE_UPDATE_COMMIT_MESSAGE := "chore: update mk-include"

GITHUB_CLI_VERSION ?= 0.11.1

ifneq ($(filter $(MK_INCLUDE_UPDATE_BRANCH),$(SEMAPHORE_GIT_PR_BRANCH) $(SEMAPHORE_GIT_BRANCH)),)
TRIGGER_PR := false
endif

ifeq ($(CI_TEST),true)
GIT := echo git
REMOVE := echo rm
GH := echo gh
else
GIT := git
REMOVE := rm
GH := gh
endif

# docker repository
JFROG_DOCKER_REPO := confluent-docker.jfrog.io
JFROG_DOCKER_REPO_INTERNAL := confluent-docker-internal-dev.jfrog.io
DOCKERHUB_REPO := https://index.docker.io/v1/

ifeq ($(CI),true)
DOCKER_REPO ?= $(JFROG_DOCKER_REPO)
else
DOCKER_REPO ?= $(JFROG_DOCKER_REPO_INTERNAL)
endif

DOCKER_LOGIN ?= true
ifeq ($(DOCKER_LOGIN), true)
INIT_CI_TARGETS += docker-login-ci
endif

.PHONY: update-mk-include
update-mk-include:
	set -e ;\
	if [[ "" != $$(git status --untracked-files=no --porcelain) ]] ; then \
	echo "git must be clean to update mk-include" ;\
	exit 1 ;\
	fi ;
	@echo "update mk-include"
	$(REMOVE) -rf mk-include
	$(GIT) commit -a -m 'chore: reset mk-include'
	$(GIT) subtree add --prefix mk-include git@github.com:confluentinc/cc-mk-include.git $(MK_INCLUDE_UPDATE_VERSION) --squash
	mkdir -p $(MK_INCLUDE_DATA)
	@echo $(MK_INCLUDE_GIT_HASH) > $(MK_INCLUDE_GIT_HASH_LOCATION)
	$(GIT) add -f $(MK_INCLUDE_GIT_HASH_LOCATION)
	$(GIT) commit -m "chore: add mk-include-git-hash"

.PHONY: diff-mk-include
diff-mk-include:
ifeq ($(CI),true)
ifeq (true, $(UPDATE_MK_INCLUDE))
ifneq (false, $(TRIGGER_PR))
	@$(MAKE) install-github-cli
	export MASTER_BRANCH=$(MASTER_BRANCH) ;\
	export GIT=$(GIT) ;\
	export MK_INCLUDE_UPDATE_COMMIT_MESSAGE=$(MK_INCLUDE_UPDATE_COMMIT_MESSAGE) ;\
	export MK_INCLUDE_UPDATE_BRANCH=$(MK_INCLUDE_UPDATE_BRANCH) ;\
	export GIT_REMOTE_NAME=$(GIT_REMOTE_NAME) ;\
	export MAKE=$(MAKE) ;\
	export MK_INCLUDE_GIT_HASH=$(MK_INCLUDE_GIT_HASH) ;\
	export MK_INCLUDE_GIT_HASH_LOCATION=$(MK_INCLUDE_GIT_HASH_LOCATION) ;\
	mk-include/bin/diff-mk-include.sh ;
else
	@echo "$(MK_INCLUDE_UPDATE_BRANCH) is already trying to update the mk-include directory, will not update and file PR"
endif
else
	@echo "auto update mk-include is disabled"
endif
else
	@echo "This command is supposed to only run in CI"
endif
	@:

.PHONY: install-github-cli
install-github-cli:
	export GITHUB_CLI_VERSION=$(GITHUB_CLI_VERSION) ;\
	mk-include/bin/install-github-cli.sh

.PHONY: trigger-mk-include-update-pr
trigger-mk-include-update-pr:
ifeq (true, $(UPDATE_MK_INCLUDE))
	@$(MAKE) update-mk-include
	$(GIT) push -f $(GIT_REMOTE_NAME) $(MK_INCLUDE_UPDATE_BRANCH)
	@echo "update cc-mk-include finished, open update PR"
	$(GH) pr create -B $(MASTER_BRANCH) -b "update mk-include" -t $(MK_INCLUDE_UPDATE_COMMIT_MESSAGE)
endif
	@:

.PHONY: add-github-templates
add-github-templates:
	$(eval project_root := $(shell git rev-parse --show-toplevel))
	$(eval mk_include_relative_path := ../mk-include)
	$(if $(wildcard $(project_root)/.github/pull_request_template.md),$(an error ".github/pull_request_template.md already exists, try deleting it"),)
	$(if $(filter $(BRANCH_NAME),$(MASTER_BRANCH)),$(error "You must run this command from a branch: 'git checkout -b add-github-pr-template'"),)

	@mkdir -p $(project_root)/.github
	@ln -s $(mk_include_relative_path)/.github/pull_request_template.md $(project_root)/.github
	@git add $(project_root)/.github/pull_request_template.md
	@git commit \
		-m "Add .github template for PRs $(CI_SKIP)" \
		-m "Adds the .github/pull_request_template.md as described in [1]" \
		-m "linking to the shared template in \`mk-include\`." \
		-m "" \
		-m "[1] https://github.com/confluentinc/cc-mk-include/pull/113"

	@git show
	@echo "Template added."
	@echo "Create PR with 'git push && git log --format=%B -n 1 | hub pull-request -F -'"

.PHONY: add-paas-github-templates
add-paas-github-templates:
	$(eval project_root := $(shell git rev-parse --show-toplevel))
	$(eval mk_include_relative_path := ../mk-include)
	$(if $(wildcard $(project_root)/.github/pull_request_template.md),$(an error ".github/pull_request_template.md already exists, try deleting it"),)
	$(if $(filter $(BRANCH_NAME),$(MASTER_BRANCH)),$(error "You must run this command from a branch: 'git checkout -b add-github-pr-template'"),)

	@mkdir -p $(project_root)/.github
	@ln -s $(mk_include_relative_path)/.github/paas_pull_request_template.md $(project_root)/.github/pull_request_template.md
	@git add $(project_root)/.github/pull_request_template.md
	@git commit \
		-m "Add .github template for PRs $(CI_SKIP)" \
		-m "Adds the .github/pull_request_template.md as described in [1]" \
		-m "linking to the shared template in \`mk-include\`." \
		-m "" \
		-m "[1] https://github.com/confluentinc/cc-mk-include/pull/113"

	@git show
	@echo "Template added."
	@echo "Create PR with 'git push && git log --format=%B -n 1 | hub pull-request -F -'"

.PHONY: add-auto-merge-templates
add-auto-merge-templates:
	$(eval project_root := $(shell git rev-parse --show-toplevel))
	$(eval mk_include_relative_path := $(project_root)/mk-include)
	$(if $(wildcard $(project_root)/.github/bulldozer.yml),$(an error ".github/bulldozer.yml already exists, try deleting it"),)
	$(if $(filter $(BRANCH_NAME),$(MASTER_BRANCH)),$(error "You must run this command from a branch: 'git checkout -b add-github-pr-template'"),)

	@mkdir -p $(project_root)/.github
	@cp $(mk_include_relative_path)/resources/bulldozer-template.yml $(project_root)/.github/bulldozer.yml
	@git add $(project_root)/.github/bulldozer.yml
	@git commit \
		-m "Add bulldozer automerge template for PRs $(CI_SKIP)"
	@git show
	@echo "Auto merge template added."
	@echo "Create PR with 'git push && git log --format=%B -n 1 | hub pull-request -F -'"

.PHONY: docker-login-ci
docker-login-ci:
ifeq ($(CI),true)
	@mkdir -p $(HOME)/.docker && touch $(HOME)/.docker/config.json
# Login to docker Artifactory
ifeq ($(DOCKER_USER)$(DOCKER_APIKEY),$(_empty))
	@echo "No docker artifactory creds are set, skip artifactory docker login"
else
	@jq -e '.auths."$(JFROG_DOCKER_REPO)"' $(HOME)/.docker/config.json 2>&1 >/dev/null || true
	@docker login $(JFROG_DOCKER_REPO) --username $(DOCKER_USER) --password $(DOCKER_APIKEY) || \
		docker login $(JFROG_DOCKER_REPO) --username $(DOCKER_USER) --password $(DOCKER_APIKEY)
	@jq -e '.auths."$(JFROG_DOCKER_REPO_INTERNAL)"' $(HOME)/.docker/config.json 2>&1 >/dev/null || true
	@docker login $(JFROG_DOCKER_REPO_INTERNAL) --username $(DOCKER_USER) --password $(DOCKER_APIKEY) || \
		docker login $(JFROG_DOCKER_REPO_INTERNAL) --username $(DOCKER_USER) --password $(DOCKER_APIKEY)
endif
# login to dockerhub as confluentsemaphore
ifeq ($(DOCKERHUB_USER)$(DOCKERHUB_APIKEY),$(_empty))
	@echo "No dockerhub creds are set, skip dockerhub docker login"
else
	@jq -e '.auths."$(DOCKERHUB_REPO)"' $(HOME)/.docker/config.json 2>&1 >/dev/null || true
	@docker login --username $(DOCKERHUB_USER) --password $(DOCKERHUB_APIKEY) || \
		docker login --username $(DOCKERHUB_USER) --password $(DOCKERHUB_APIKEY)
endif
endif


.PHONY: bats
bats:
	find . -name *.bats -exec bats {} \;

$(HOME)/.netrc:
ifeq ($(CI),true)
	$(error .netrc missing, can't authenticate to GitHub)
else
	$(shell bash -c 'echo .netrc missing, prompting for user input >&2')
	$(shell bash -c 'echo Enter Github credentials, if you use 2 factor authentication generate a personal access token for the password: https://github.com/settings/tokens >&2')
	$(eval user := $(shell bash -c 'read -p "GitHub Username: " user; echo $$user'))
	$(eval pass := $(shell bash -c 'read -s -p "GitHub Password: " pass; echo $$pass'))
	@printf "machine github.com\n\tlogin $(user)\n\tpassword $(pass)\n\nmachine api.github.com\n\tlogin $(user)\n\tpassword $(pass)\n" > $(HOME)/.netrc
	@echo
endif

ifneq ($(DOCKER_BUILDKIT),0)
export DOCKER_BUILDKIT=1

.netrc:

.ssh:

.aws:

.gitconfig:

else

.netrc: $(HOME)/.netrc
	cp $(HOME)/.netrc .netrc

.ssh: $(HOME)/.ssh
	cp -R $(HOME)/.ssh/. .ssh

.aws: $(HOME)/.aws
	cp -R $(HOME)/.aws/. .aws

.gitconfig: $(HOME)/.gitconfig
	cp $(HOME)/.gitconfig .gitconfig

endif
