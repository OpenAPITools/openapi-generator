# Defaults
GO ?= $(shell which go)# default go bin to whatever's on the path
GO_VERSION := $(shell $(GO) version)# the version of the go bin
GO_ALPINE ?= true# default to alpine images
GO_STATIC ?= true# default to static binaries
GO_BINS ?= main.go=main# format: space seperated list of source.go=output_bin
GO_OUTDIR ?= bin# default to output bins to bin/
GO_LDFLAGS ?= -X main.version=$(VERSION)# Setup LD Flags
GO_EXTRA_FLAGS ?=

GO_TEST_SETUP_CMD ?= :
GO_TEST_ARGS ?= -race -v -cover# default list of args to pass to go test
ifdef TESTS_TO_RUN
GO_TEST_ARGS += -run $(TESTS_TO_RUN)
endif
GO_TEST_PACKAGE_ARGS ?= ./...

# flags for confluent-kafka-go-dev / librdkafka on alpine
ifeq ($(GO_ALPINE),true)
GO_EXTRA_FLAGS += -tags musl
endif

# force rebuild of all packages on CI
ifeq ($(CI),true)
GO_EXTRA_FLAGS += -a
endif

# Build the listed main packages and everything they import into executables
ifeq ($(GO_STATIC),true)
GO_EXTRA_FLAGS += -tags static_all -buildmode=exe
endif

# List of all go files in project
ALL_SRC = $(shell \
	find . \
	-type d \( \
		-path ./vendor \
		-o -path ./.gomodcache \
		-o -path ./.semaphore-cache \
		-o -path ./mk-include \
	\) -prune \
	-o -name \*.go -not -name bindata.go -print \
)

# CLI Doc gen stuff
CLI_DOCS_GEN_MAINS ?= # Default to empty
CLI_DOCS_GEN_DIRS := $(dir $(CLI_DOCS_GEN_MAINS))

# Force go mod on
GO111MODULE := on
export GO111MODULE

# Mark confluentinc projects as private
GOPRIVATE ?= github.com/confluentinc/*
export GOPRIVATE

# Disable go mod changes on CI
ifeq ($(CI),true)
GO_MOD_DOWNLOAD_MODE_FLAG ?= -mod=readonly
else
GO_MOD_DOWNLOAD_MODE_FLAG ?=
endif

# Allow for opt out of module prefetching on CI
GO_PREFETCH_DEPS ?= true

GOPATH ?= $(shell $(GO) env GOPATH)

GO_BUILD_TARGET ?= build-go
GO_TEST_TARGET ?= lint-go test-go
GO_CLEAN_TARGET ?= clean-go

ifeq ($(GO_PREFETCH_DEPS),true)
INIT_CI_TARGETS += deps
endif
BUILD_TARGETS += $(GO_BUILD_TARGET) gen-cli-docs-go
TEST_TARGETS += $(GO_TEST_TARGET)
CLEAN_TARGETS += $(GO_CLEAN_TARGET)
RELEASE_PRECOMMIT += commit-cli-docs-go
RELEASE_POSTCOMMIT += $(GO_DOWNSTREAM_DEPS)

GO_BINDATA_VERSION := 3.11.0
GO_BINDATA_OPTIONS ?=
GO_BINDATA_OUTPUT ?= deploy/bindata.go

ifeq ($(CI),true)
# Override the DB_URL for go tests that need access to postgres
DB_URL ?= postgres://$(DATABASE_POSTGRESQL_USERNAME):$(DATABASE_POSTGRESQL_PASSWORD)@127.0.0.1:5432/mothership?sslmode=disable
export DB_URL
endif

.PHONY: show-go
## Show Go Variables
show-go:
	@echo "GO: $(GO)"
	@echo "GO_VERSION: $(GO_VERSION)"
	@echo "GO_BINS: $(GO_BINS)"
	@echo "GO_OUTDIR: $(GO_OUTDIR)"
	@echo "GO_LDFLAGS: $(GO_LDFLAGS)"
	@echo "GO_EXTRA_FLAGS: $(GO_EXTRA_FLAGS)"
	@echo "GO_MOD_DOWNLOAD_MODE_FLAG: $(GO_MOD_DOWNLOAD_MODE_FLAG)"
	@echo "GO_ALPINE: $(GO_ALPINE)"
	@echo "GO_STATIC: $(GO_STATIC)"
	@echo "GO111MODULE: $(GO111MODULE)"
	@echo "GOPATH: $(GOPATH)"
	@echo "GO_BINDATA_VERSION: $(GO_BINDATA_VERSION)"
	@echo "DB_URL: $(DB_URL)"
	@echo "GO_DOWNSTREAM_DEPS: $(GO_DOWNSTREAM_DEPS)"
	@echo "CLI_DOCS_GEN_MAINS: $(CLI_DOCS_GEN_MAINS)"
	@echo "GO_PREFETCH_DEPS: $(GO_PREFETCH_DEPS)"
	@echo "GO_TEST_ARGS: $(GO_TEST_ARGS)"
	@echo "GO_TEST_PACKAGE_ARGS: $(GO_TEST_PACKAGE_ARGS)"


.PHONY: clean-go
clean-go:
ifeq ($(abspath $(GO_OUTDIR)),$(abspath $(BIN_PATH)))
	@echo "WARNING: Your project is deleting BIN_PATH contents during clean-go."
	@echo "BIN_PATH: $(BIN_PATH), abs: $(abspath $(BIN_PATH))"
	@echo "CI_BIN: $(CI_BIN), abs: $(abspath $(CI_BIN))"
	@echo "GO_OUTDIR: $(GO_OUTDIR), abs: $(abspath $(GO_OUTDIR))"
endif
	rm -rf $(SERVICE_NAME) $(GO_OUTDIR)

.PHONY: vet
vet:
	$(GO) list $(GO_MOD_DOWNLOAD_MODE_FLAG) $(GO_TEST_PACKAGE_ARGS) | grep -v vendor | xargs $(GO) vet $(GO_MOD_DOWNLOAD_MODE_FLAG)

.PHONY: deps
## fetch any dependencies - go mod download is opt out
deps: $(HOME)/.hgrc $(GO_EXTRA_DEPS)
	$(GO) mod download
	$(GO) mod verify
ifeq ($(GO_USE_VENDOR),-mod=vendor)
	$(GO) mod vendor
endif

$(HOME)/.hgrc:
	echo -e '[ui]\ntls = False' > $@

.gomodcache:
	mkdir .gomodcache || true

.PHONY: lint-go
## Lints (gofmt)
lint-go: $(GO_EXTRA_LINT)
	@test -z "$$(gofmt -e -s -l -d $(ALL_SRC) | tee /dev/tty)"

.PHONY: fmt
# Format entire codebase
fmt:
	@gofmt -e -s -l -w $(ALL_SRC)

.PHONY: build-go
## Build just the go project
build-go: go-bindata $(GO_BINS)
$(GO_BINS):
	$(eval split := $(subst =, ,$(@)))
	$(GO) build $(GO_USE_VENDOR) $(GO_MOD_DOWNLOAD_MODE_FLAG) -o $(GO_OUTDIR)/$(word 2,$(split)) -ldflags "$(GO_LDFLAGS)" $(GO_EXTRA_FLAGS) $(word 1,$(split))

.PHONY: test-go
## Run Go Tests and Vet code
test-go: vet
	test -f coverage.txt && truncate -s 0 coverage.txt || true
	set -o pipefail && $(GO_TEST_SETUP_CMD) &&  $(GO) test $(GO_MOD_DOWNLOAD_MODE_FLAG) -coverprofile=coverage.txt $(GO_TEST_ARGS) $(GO_TEST_PACKAGE_ARGS) -json > >($(MK_INCLUDE_BIN)/decode_test2json.py) 2> >($(MK_INCLUDE_BIN)/color_errors.py >&2)

.PHONY: test-go-goland-debug
## Vet code, Launch Go Tests and wait for GoLand debugger to attach on ${DEBUG_PORT}
test-go-goland-debug: vet
ifeq ($(GO_TEST_PACKAGE_ARGS),./...)
	@echo "Error: must specify a test/package using GO_TEST_PACKAGE_ARGS= on the commandline"
	@echo "Usage: GO_TEST_PACKAGE_ARGS=./test/connect/... make $@" && exit 1
endif
	test -f coverage.txt && truncate -s 0 coverage.txt || true
	go test -c $(GO_MOD_DOWNLOAD_MODE_FLAG) $(GO_TEST_PACKAGE_ARGS) -gcflags='all=-N -l'
	$(eval go_test_binary := $(shell echo "$(GO_TEST_PACKAGE_ARGS)" | awk -F/ '{print "./"$$3"."$$2}'))
	$(eval prefixed_go_test_args := $(shell echo "$(GO_TEST_ARGS)" |  sed 's/-/-test./g'))
	$(eval goland_dlv_cmd := /Applications/GoLand.app/Contents/plugins/go/lib/dlv/mac/dlv --listen=0.0.0.0:12345 --headless=true --api-version=2 --check-go-version=false --only-same-user=false)
ifneq ($(GOLAND_PORT),)
	$(eval goland_dlv_cmd := $(subst 12345,$(GOLAND_PORT),$(goland_dlv_cmd)))
endif
	set -o pipefail && go tool test2json -t ${goland_dlv_cmd} exec ${go_test_binary} -- ${prefixed_go_test_args} | $(MK_INCLUDE_BIN)/decode_test2json.py

.PHONY: generate
## Run go generate
generate:
	$(GO) generate $(GO_MOD_DOWNLOAD_MODE_FLAG)

SEED_POSTGRES_URL ?= postgres://

.PHONY: seed-local-mothership
## Seed local mothership DB. Optionally set SEED_POSTGRES_URL for base postgres url
seed-local-mothership:
	@echo "Seeding postgres in 'SEED_POSTGRES_URL=${SEED_POSTGRES_URL}'. Set SEED_POSTGRES_URL to override"
	psql -P pager=off ${SEED_POSTGRES_URL}/postgres -c 'DROP DATABASE IF EXISTS mothership;'
	psql -P pager=off ${SEED_POSTGRES_URL}/postgres -c 'CREATE DATABASE mothership;'
	psql -P pager=off ${SEED_POSTGRES_URL}/mothership -f mk-include/seed-db/mothership-seed.sql

.PHONY: install-go-bindata
GO_BINDATA_INSTALLED_VERSION := $(shell $(BIN_PATH)/go-bindata -version 2>/dev/null | head -n 1 | awk '{print $$2}' | xargs)
install-go-bindata:
	@echo "go-bindata installed version: $(GO_BINDATA_INSTALLED_VERSION)"
	@echo "go-bindata want version: $(GO_BINDATA_VERSION)"
ifneq ($(GO_BINDATA_INSTALLED_VERSION),$(GO_BINDATA_VERSION))
	mkdir -p $(BIN_PATH)
	curl -L -o $(BIN_PATH)/go-bindata https://github.com/kevinburke/go-bindata/releases/download/v$(GO_BINDATA_VERSION)/go-bindata-$(shell $(GO) env GOOS)-$(shell $(GO) env GOARCH)
	chmod +x $(BIN_PATH)/go-bindata
endif

.PHONY: go-bindata
ifneq ($(GO_BINDATA_OPTIONS),)
## Run go-bindata for project
go-bindata: install-go-bindata install-github-cli
	$(BIN_PATH)/go-bindata $(GO_BINDATA_OPTIONS)
	@echo
	@echo "Here is the list of static assets bundled by go-bindata:"
	@sed -n '/\/\/ sources:/,/^$$/p' $(GO_BINDATA_OUTPUT)
ifeq ($(CI),true)
ifeq ($(findstring pull-request,$(BRANCH_NAME) $(SEMAPHORE_GIT_REF_TYPE)),pull-request)
	git diff --exit-code --name-status || \
		(echo "ERROR: cannot commit changes back to a fork, please run go-bindata locally and commit the changes" && \
		gh api -XPOST repos/${SEMAPHORE_GIT_PR_SLUG}/issues/${SEMAPHORE_GIT_PR_NUMBER}/comments -F body=@mk-include/resources/gh-comment-go-bindata.md && \
		exit 1)
else
	git diff --exit-code --name-status || \
		(git add $(GO_BINDATA_OUTPUT) && \
		git commit -m 'chore: updating bindata' && \
		git push $(GIT_REMOTE_NAME) $(BRANCH_NAME))
endif
endif
else
go-bindata:
endif

.PHONY: gen-cli-docs-go $(CLI_DOCS_GEN_MAINS)
## Generate go cli docs if generator file is specified
gen-cli-docs-go: $(CLI_DOCS_GEN_MAINS)
$(CLI_DOCS_GEN_MAINS):
	$(GO) run $(GO_MOD_DOWNLOAD_MODE_FLAG) $@

.PHONY: commit-cli-docs-go $(CLI_DOCS_GEN_DIRS)
commit-cli-docs-go: $(CLI_DOCS_GEN_DIRS)
	@:
$(CLI_DOCS_GEN_DIRS):
	git diff --exit-code --name-status $@ || \
		(git add $@ && \
		git commit -m 'chore: updating cli docs [ci skip]')

.PHONY: go-update-deps
## Update dependencies (go get -u)
go-update-deps:
ifeq ($(HOTFIX),true)
	$(GO) get -u=patch
else
	$(GO) get -u
endif
	$(GO) mod tidy

.PHONY: go-update-dep
## Update single dependency, specify with DEP=
go-update-dep:
ifeq ($(DEP),)
	@echo "Error: must specify DEP= on the commandline"
	@echo "Usage: $(MAKE) go-update-dep DEP=github.com/confluentinc/example@v1.2.3"
else
	$(GO) get $(DEP)
	$(GO) mod tidy
endif

.PHONY: go-commit-deps
## Commit (and push) updated go deps.
## NOTE: Some repos have go.sum in `.gitignore`. We use `git ls-files` to only
## add go.sum if it's tracked by git.
go-commit-deps:
	git diff --exit-code --name-status || \
		(git add $$(git ls-files go.mod go.sum) && \
		git commit -m 'chore: $(UPSTREAM_MOD):$(UPSTREAM_VERSION) updating go deps' && \
		git push $(GIT_REMOTE_NAME) $(GIT_BRANCH_NAME))

.PHONY: $(GO_DOWNSTREAM_DEPS)
$(GO_DOWNSTREAM_DEPS):
ifeq ($(HOTFIX),true)
	@echo "Skipping bumping downstream go dep $@ on hotfix branch"
else ifeq ($(BUMP),major)
	@echo "Skipping bumping downstream go dep $@ with major version bump"
else
	git clone git@github.com:confluentinc/$@.git $@
	$(MAKE) $(MAKE_ARGS) -C $@ go-update-dep go-commit-deps \
		DEP=$(shell grep module go.mod | awk '{print $$2}')@$(BUMPED_VERSION) \
		UPSTREAM_MOD=$(SERVICE_NAME) \
		UPSTREAM_VERSION=$(BUMPED_VERSION)
	rm -rf $@
endif
