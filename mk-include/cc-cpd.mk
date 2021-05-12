# Default version to install, new enough to self update quickly
CPD_VERSION ?= baseline
CPD_UPDATE ?= true
GCLOUD_INSTALL ?= true

ifeq ($(GCLOUD_INSTALL),true)
INIT_CI_TARGETS += gcloud-install
endif
INIT_CI_TARGETS += cpd-update
CLEAN_TARGETS += clean-cc-system-tests

# Set path for cpd binary
CPD_PATH := $(BIN_PATH)/cpd

POOL_TAG ?= random
POOL_NAME ?= ci

CPD_GKE = ""

# Create Arguments
CPD_CR_ARGS ?= --deploy=false --pool-name $(POOL_NAME) --pool-tag $(POOL_TAG)

# to check if we see `cc-` pattern
CHART_PREFIX := $(shell echo $(CHART_NAME) | head -c 3)

# Local CPD Deploy Arguments
## deploy arguments used when deploying local chart and/or image to cpd
## this is used in scenarios like cpd gating for cc-<service> repo PR
ifeq ($(CHART_NAME),cc-umbrella-chart)
CPD_DEP_ARGS ?= --umbrella-chart-path='$(PWD)/charts/cc-umbrella-chart'
else ifeq ($(CHART_PREFIX), cc-)
CPD_DEP_ARGS ?= --override-subchart-path='$(CHART_NAME)=$(PWD)/charts/$(CHART_NAME)' --set="$(CHART_NAME).image.tag=$(IMAGE_VERSION_NO_V),$(CHART_NAME).image.repository=$(DOCKER_REPO)/$(IMAGE_REPO)"
else
CPD_DEP_ARGS ?= ""
endif

# system test variables
CC_SYSTEM_TESTS_URI ?= git@github.com:confluentinc/cc-system-tests.git
CC_SYSTEM_TESTS_REF ?= $(shell (test -f CC_SYSTEM_TESTS_VERSION && head -n 1 CC_SYSTEM_TESTS_VERSION) || echo master)

.PHONY: show-cpd
## Show cpd vars
show-cpd:
	@echo "cpd version: $(CPD_VERSION)"
	@echo "cpd path: $(CPD_PATH)"
	@echo "cpd name: $(CPD_NAME)"
	@echo "cpd expire: $(CPD_EXPIRE)"
	@echo "cpd create args: $(CPD_CR_ARGS)"
	@echo "cpd deploy args: $(CPD_DEP_ARGS)"
	@echo "cpd running count: $(CPD_RUNNING_COUNT)"
	@echo "cc-system-tests run: $(RUN_SYSTEM_TESTS)"
	@echo "cc-system-tests uri: $(CC_SYSTEM_TESTS_URI)"
	@echo "cc-system-tests ref: $(CC_SYSTEM_TESTS_REF)"
	@echo "cc-system-tests delete: $(DELETE_CPD)"

.PHONY: gcloud-install
# https://cloud.google.com/sdk/docs/downloads-apt-get - updated Dec 10.
# https://askubuntu.com/questions/1135822 - ppa:jonathonf/python-2.7
# https://launchpad.net/~jonathonf/+archive/ubuntu/python-2.7
gcloud-install:
ifeq ($(CI),true)
	sudo rm -f /etc/apt/sources.list.d/gcloud-source.list
	sudo apt-get -y install apt-transport-https ca-certificates gnupg
	echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
	curl https://www.mongodb.org/static/pgp/server-3.4.asc | sudo apt-key add -
	curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
	sudo apt-get update
	install-package --update google-cloud-sdk kubectl || sudo apt-get install google-cloud-sdk kubectl
	gcloud config set project cloud-private-dev
	gcloud config set account semaphore@cloud-private-dev.iam.gserviceaccount.com
	gcloud auth activate-service-account --key-file ~/.config/gcloud/application_default_credentials.json
endif

.PHONY: cpd-install
# Install cpd if it's not installed
cpd-install:
	@if [ ! -f $(CPD_PATH) ]; then \
		echo "## Installing CPD binary"; \
		gsutil cp gs://cloud-confluent-bin/cpd/cpd-$(CPD_VERSION)-$(shell go env GOOS)-$(shell go env GOARCH) $(CPD_PATH); \
		chmod +x $(CPD_PATH); \
	fi

.PHONY: cpd-update
# Update cpd if needed, install if missing
cpd-update: cpd-install
ifeq ($(CPD_UPDATE),true)
	echo "## Updating CPD binary to latest";
	$(CPD_PATH) update --yes
endif

.PHONY: cpd-priv-create-if-missing
cpd-priv-create-if-missing:
	@if [ ! `kubectl config current-context` ]; then \
		echo "## Try to allocate a CPD from the ci pool"; \
		$(CPD_PATH) pool new $(CPD_CR_ARGS); \
		kubectl config current-context; \
	else \
		echo "Already allocated one CPD $(kubectl config current-context)"; \
	fi

# This is for CPD gating
CPD_HALYARD_DEPLOYER_ADDRESS ?= halyard-deployer.cpd.halyard.confluent.cloud:9090
CPD_HALYARD_RELEASE_ADDRESS ?= halyard-release.cpd.halyard.confluent.cloud:9090
CPD_HALYARD_RENDERER_ADDRESS ?= halyard-renderer.cpd.halyard.confluent.cloud:9090
CPD_HALCTL_ARGS := --vault-login-path auth/app/devel/login
CPD_HALCTL_ARGS += --vault-oidc-role halyard-devel
CPD_HALYARD_INSTALL_SERVICE_ENVS ?= $(CHART_NAME)=cpd

.PHONY: cpd-deploy-local
## Deploy local chart to cpd cluster
cpd-deploy-local: cpd-update helm-update-repo cpd-priv-create-if-missing
ifneq ($(wildcard .halyard/*.yaml),)
ifeq ($(CI),true)
	$(eval CPD_HAL_TMPDIR := $(shell mktemp -d 2>/dev/null || mktemp -d -t 'halyard'))
else
	$(eval CPD_HAL_TMPDIR := $(PWD))
endif
	@echo "## Updating halyard spec with dirty image";
	$(CPD_PATH) set-halyard-values \
		--set "image.tag=$(IMAGE_VERSION_NO_V)" \
		--set "image.repository=$(DOCKER_REPO)/$(IMAGE_REPO)" \
		--file $(wildcard .halyard/*.yaml)
	@echo "## Updating halyard with new halyard specs";
	HALYARD_DEPLOYER_ADDRESS=$(CPD_HALYARD_DEPLOYER_ADDRESS) \
	HALYARD_RELEASE_ADDRESS=$(CPD_HALYARD_RELEASE_ADDRESS) \
	HALYARD_RENDERER_ADDRESS=$(CPD_HALYARD_RENDERER_ADDRESS) \
	CPD_CLUSTER_ID=`kubectl config current-context` \
	HALYARD_INSTALL_SERVICE_ENVS=$(CPD_HALYARD_INSTALL_SERVICE_ENVS) \
	HALYARD_SOURCE_VERSION=$(CHART_VERSION) \
	HALCTL_ARGS="$(CPD_HALCTL_ARGS)" \
	HAL_TMPDIR="$(CPD_HAL_TMPDIR)" \
	$(MAKE) $(MAKE_ARGS) halyard-cpd-publish-dirty
	@echo "## Restore halyard spec";
	git reset $(wildcard .halyard/*.yaml)
	git checkout -f $(wildcard .halyard/*.yaml)
	@echo "## Deploying umbrella charts to CPD cluster without dirty service";
	$(CPD_PATH) priv dep --id `kubectl config current-context` --set="$(CHART_NAME).enabled=false"
	@echo "## Inspect CPD cluster";
	$(CPD_PATH) debug --id `kubectl config current-context` || true
	@echo "## Install dirty service with halyard and check environment version";
	HALYARD_DEPLOYER_ADDRESS=$(CPD_HALYARD_DEPLOYER_ADDRESS) \
	HALYARD_RELEASE_ADDRESS=$(CPD_HALYARD_RELEASE_ADDRESS) \
	HALYARD_RENDERER_ADDRESS=$(CPD_HALYARD_RENDERER_ADDRESS) \
	CPD_CLUSTER_ID=`kubectl config current-context` \
	HALYARD_INSTALL_SERVICE_ENVS=$(CPD_HALYARD_INSTALL_SERVICE_ENVS) \
	HALYARD_SOURCE_VERSION=$(CHART_VERSION) \
	HALCTL_ARGS="$(CPD_HALCTL_ARGS)" \
	HAL_TMPDIR="$(CPD_HAL_TMPDIR)" \
	$(MAKE) $(MAKE_ARGS) halyard-cpd-install-dirty
else
	@echo "## Deploying local charts to CPD cluster (not migrated to halyard yet)";
	$(CPD_PATH) priv dep --id `kubectl config current-context` $(CPD_DEP_ARGS)
endif

.PHONY: cpd-destroy
## Clean up all cpd clusters
cpd-destroy:
	@if [ `kubectl config current-context 2> /dev/null` ]; then \
		echo "## Try to destroy CPD cluster (logs tailed)"; \
		$(CPD_PATH) pool free --id `kubectl config current-context` 2>&1 | tail -5; \
	fi

.cc-system-tests:
	git clone $(CC_SYSTEM_TESTS_URI) .cc-system-tests

.PHONY: checkout-cc-system-tests
checkout-cc-system-tests: .cc-system-tests
	@echo "## Checking out cc-system-tests"
	git -C ./.cc-system-tests fetch origin
	git -C ./.cc-system-tests checkout $(CC_SYSTEM_TESTS_REF)
	git -C ./.cc-system-tests merge origin/$(CC_SYSTEM_TESTS_REF)
	@echo "## cc-system-tests last commit:"
	@git -C ./.cc-system-tests log -n 1

define _newline


endef

#####################################
# Run tests on CPD
#####################################

ifndef TESTS_TO_RUN
# Currently TestAccountTestSuite are hardcoded as these seems to be the stable set of tests.
# These tests are run using go test -run, so this is an example on how to run different tests.
TESTS_TO_RUN ?= "TestFoobarTestSuite"
endif

ifndef TESTS_SOFT_FAILURE
TESTS_SOFT_FAILURE ?= false
endif

.PHONY: system-tests-on-cpd
## Run cc-system tests
system-tests-on-cpd:
	. mk-include/bin/vault-setup
ifeq ($(TESTS_SOFT_FAILURE), true)
	$(MAKE) $(MAKE_ARGS) _run-cc-system-tests || ( $(CPD_PATH) debug --id `kubectl config current-context`; exit 0 )
	@echo "## TESTS_SOFT_FAILURE is set, ignoring test failures, if any."
else
	$(MAKE) $(MAKE_ARGS) _run-cc-system-tests || ( $(CPD_PATH) debug --id `kubectl config current-context`; exit 1 )
endif

CC_SYSTEM_TEST_CHECKOUT_DIR = ./.cc-system-tests
CC_SYSTEM_TEST_ENV_SECRETS = $(CC_SYSTEM_TEST_CHECKOUT_DIR)/.profile-with-secrets

# This is a hidden target, used only from the system-tests-on-cpd.
.PHONY: _run-cc-system-tests
_run-cc-system-tests: checkout-cc-system-tests cpd-deploy-local
	@echo "## Exporting CPD environment bash profile."
	set -o pipefail && $(CPD_PATH) priv testenv --id `kubectl config current-context` > $(CC_SYSTEM_TEST_ENV_SECRETS)
	@echo "## Running cc-system-tests's $(MAKE) init-env."
	source $(CC_SYSTEM_TEST_ENV_SECRETS) && $(MAKE) -C $(CC_SYSTEM_TEST_CHECKOUT_DIR) init-env
	@echo "## Show debug info about CPD cluster."
	$(CPD_PATH) debug --id `kubectl config current-context` || true
	@echo "## Running cc-system-tests tests."
	source $(CC_SYSTEM_TEST_ENV_SECRETS) && TEST_REPORT_FILE="$(BUILD_DIR)/ci-gating/TEST-report.xml" TESTS_TO_RUN='$(TESTS_TO_RUN)' $(MAKE) -C $(CC_SYSTEM_TEST_CHECKOUT_DIR) test


.PHONY: clean-cc-system-tests
## Clean up .cc-system-tests folder
clean-cc-system-tests:
	rm -rf $(CC_SYSTEM_TEST_CHECKOUT_DIR)
