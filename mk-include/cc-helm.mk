# Note: you also need to include cc-cpd.mk as CPD is used as CLI to modify chart files.

CHART_NAME ?=
CHARTS_ROOT ?= charts
IMAGE_VERSION ?= 0.0.0

HELM_VERSION ?= v3.3.4
HELM_TGZ := https://get.helm.sh/helm-$(HELM_VERSION)-linux-amd64.tar.gz
HELM_BINARY := helm
HELM_ARTIFACTORY_VERSION ?= 1.0.1
HELM_REPO := https://confluent.jfrog.io/confluent/helm-cloud

INIT_CI_TARGETS += helm-setup-ci
BUILD_TARGETS += helm-package
TEST_TARGETS += helm-lint
CLEAN_TARGETS += helm-clean
RELEASE_PRECOMMIT += helm-set-bumped-version helm-update-floating-deps
RELEASE_MAKE_TARGETS += helm-release $(HELM_DOWNSTREAM_CHARTS)

CHART_VERSION := $(VERSION_NO_V)
BUMPED_CHART_VERSION := $(BUMPED_CLEAN_VERSION)

CHART_NAMESPACE := $(CHART_NAME)-dev
CHART_RELEASE_NAME := $(CHART_NAME)-dev
CHART_LOCAL_PATH := $(CHARTS_ROOT)/$(CHART_NAME)

ifeq ($(HOST_OS),darwin)
HELM_REPO_CACHE := $(HOME)/Library/Caches/helm/repository
else
HELM_REPO_CACHE := $(HOME)/.cache/helm/repository
endif

# Include extra args for helm dep build
HELM_DEP_BUILD_EXTRA_ARGS ?=
# Target to use to push packaged Helm chart to Artifactory
HELM_PUSH_TARGET ?= helm-push-via-cpd

# requirements.lock - helm 2 / apiversion v1 charts.
# Chart.lock - helm 3 / apiversion v2 charts.
CHART_LOCK_FILE := $(wildcard $(CHART_LOCAL_PATH)/requirements.lock $(CHART_LOCAL_PATH)/Chart.lock)

# The main Chart.yaml file
# helm 3 / apiversion v2 charts contains dependencies aka pinned versions / version ranges.
CHART_YAML_FILE := $(CHART_LOCAL_PATH)/Chart.yaml

.PHONY: show-helm
## Show helm variables
show-helm:
	@echo "HELM_VERSION: $(HELM_VERSION)"
	@echo "CHART_NAME: $(CHART_NAME)"
	@echo "IMAGE_VERSION: $(IMAGE_VERSION)"
	@echo "CHART_VERSION: $(VERSION_NO_V)"
	@echo "CHART_URL: $(HELM_REPO)/$(CHART_NAME)/$(CHART_VERSION)"
	@echo "BUMPED_CHART_VERSION: $(BUMPED_CLEAN_VERSION)"
	@echo "HELM_DOWNSTREAM_CHARTS: $(HELM_DOWNSTREAM_CHARTS)"

.PHONY: helm-kube-config
helm-kube-config:
	chmod go-rw ~/.kube/config

.PHONY: helm-clean
helm-clean:
	@echo 💬 uninstalling chart release $(CHART_RELEASE_NAME) from namespace $(CHART_NAMESPACE)
	$(HELM_BINARY) uninstall --namespace $(CHART_NAMESPACE) $(CHART_RELEASE_NAME)

.PHONY: helm-lint
## Lint helm chart with values from $(CHART_LOCAL_PATH)/lint.yaml (if present)
helm-lint:
	test -f $(CHART_LOCAL_PATH)/lint.yaml && VALUES="--values $(CHART_LOCAL_PATH)/lint.yaml" ;\
	echo "💬 running helm lint for chart and subchart (informational)"; \
	$(HELM_BINARY) lint --with-subcharts $(CHART_LOCAL_PATH) $$VALUES || true; \
	echo "💬 running helm lint again for chart only (validation)"; \
	$(HELM_BINARY) lint $(CHART_LOCAL_PATH) $$VALUES


.PHONY: helm-deploy-local
## Deploy helm to current kube context with values set to local.yaml
helm-deploy-local:
	@echo 💬 installing chart release $(CHART_RELEASE_NAME) into namespace $(CHART_NAMESPACE)
	# Note: do not use --debug as this leaks values to console or log, aka secrets.
	$(HELM_BINARY) upgrade --install $(CHART_RELEASE_NAME) $(CHART_LOCAL_PATH) \
	--namespace $(CHART_NAMESPACE) --create-namespace \
	--set namespace=$(CHART_NAMESPACE) \
	-f $(CHARTS_ROOT)/values/local.yaml \
	--set image.tag=$(IMAGE_VERSION) $(HELM_ARGS)

.PHONY: helm-set-bumped-version
helm-set-bumped-version:
	@echo 💬 updating chart version to $(BUMPED_CHART_VERSION)
	$(CPD_PATH) helm setver --chart $(CHART_YAML_FILE) --version $(BUMPED_CHART_VERSION) || true
	git add $(CHART_YAML_FILE) || true

.PHONY: helm-set-version
helm-set-version:
	@echo ❌ target $@ is DEPRECATED - please update your Makefiles.; false

.PHONY: helm-release-local
## Set the version to the current un-bumped version and package
helm-release-local: helm-release

$(HELM_REPO_CACHE)/helm-cloud-index.yaml:
# Adds the helm-cloud helm repo if missing
	@echo 💬 helm repo helm-cloud repo missing, adding...
	@echo helm repo helm-cloud repo missing, adding...
ifeq ($(HELM_USER)$(HELM_APIKEY),$(_empty))
ifeq ($(CI),true)
	@echo ❌ HELM_USER and HELM_APIKEY must be set on CI
	false
else
	$(eval user := $(shell bash -c 'read -p "Artifactory Email: " user; echo $$user'))
	$(eval pass := $(shell bash -c 'read -p "Artifactory API Key: " pass; echo $$pass'))
	@$(HELM_BINARY) repo add helm-cloud $(HELM_REPO) --username $(user) --password $(pass)
endif
else
	@$(HELM_BINARY) repo add helm-cloud $(HELM_REPO) --username $(HELM_USER) --password $(HELM_APIKEY)
endif

$(HELM_REPO_CACHE)/bitnami-index.yaml:
	@echo 💬 helm repo bitnami repo missing, adding...
	@$(HELM_BINARY) repo add bitnami https://charts.bitnami.com/bitnami

$(HELM_REPO_CACHE)/stable-index.yaml:
	@echo 💬 helm repo stable repo missing, adding...
	@$(HELM_BINARY) repo add stable https://charts.helm.sh/stable

.PHONY: helm-update-repo
helm-update-repo: $(HELM_REPO_CACHE)/helm-cloud-index.yaml $(HELM_REPO_CACHE)/bitnami-index.yaml $(HELM_REPO_CACHE)/stable-index.yaml
	@echo 💬 updating index / cache of helm repos
	@$(HELM_BINARY) repo update

.PHONY: helm-install-deps
## Install subchart files in charts/ based on Chart.lock file
helm-install-deps: $(HELM_REPO_CACHE)/helm-cloud-index.yaml $(HELM_REPO_CACHE)/bitnami-index.yaml $(HELM_REPO_CACHE)/stable-index.yaml
	@echo 💬 building charts/ directory from Chart.lock
	$(HELM_BINARY) dep build $(CHART_LOCAL_PATH) $(HELM_DEP_BUILD_EXTRA_ARGS)

.PHONY: helm-update-deps
helm-update-deps:
	@echo ❌ target $@ is DEPRECATED - please update your Makefiles.; false

.PHONY: helm-update-floating-deps
## Update floating subchart versions that match the semantic version ranges in Chart.yaml
helm-update-floating-deps:
	@echo 💬 updating floating chart dependencies and updating lock file
	$(HELM_BINARY) dep update $(CHART_LOCAL_PATH)
	git add $(CHART_LOCK_FILE) || true

.PHONY: helm-pin-dependency-from-upstream
## Pin the upstream chart version in Chart.yaml
helm-pin-dependency-from-upstream:
	@echo 💬 updating chart dependency $(UPSTREAM_CHART) with pinned version $(UPSTREAM_VERSION)
	$(CPD_PATH) helm pin-dependency-version --chart $(CHART_LOCAL_PATH) --name $(UPSTREAM_CHART) --version $(UPSTREAM_VERSION)
	$(MAKE) $(MAKE_ARGS) helm-update-floating-deps
	git add $(CHART_YAML_FILE) $(CHART_LOCK_FILE) || true

.PHONY: helm-add-requirements
helm-add-requirements:
	@echo ❌ target $@ is DEPRECATED - please update your Makefiles.; false

.PHONY: helm-package
## Build helm package at the current version
helm-package: helm-install-deps
	mkdir -p $(CHARTS_ROOT)/package
	rm -rf $(CHARTS_ROOT)/package/$(CHART_NAME)-$(CHART_VERSION).tgz
	@echo 💬 build chart package $(CHART_NAME)-$(CHART_VERSION).tgz
	$(HELM_BINARY) package --version "$(CHART_VERSION)" $(CHART_LOCAL_PATH) -d $(CHARTS_ROOT)/package

.PHONY: helm-release
helm-release: helm-package helm-push-artifactory

.PHONY: helm-push-artifactory
# push-artifactory takes up to 20 seconds to update helm-repo and reflect in helm repo update
helm-push-artifactory: $(HELM_REPO_CACHE)/helm-cloud-index.yaml
	$(HELM_BINARY) plugin list | grep push-artifactory | grep -q "$(HELM_ARTIFACTORY_VERSION)" || helm plugin install https://github.com/belitre/helm-push-artifactory-plugin --version "v$(HELM_ARTIFACTORY_VERSION)"
	$(HELM_BINARY) repo update
	@if ! $(HELM_BINARY) search repo --devel "$(CHART_NAME)" --version "$(CHART_VERSION)" | grep -q "$(CHART_VERSION)"; then\
		echo 💬 uploading chart package $(CHART_NAME)-$(CHART_VERSION).tgz;\
		$(HELM_BINARY) push-artifactory --skip-reindex $(CHARTS_ROOT)/package/$(CHART_NAME)-$(CHART_VERSION).tgz helm-cloud;\
	else\
		echo 💬 Chart $(CHART_NAME) with version $(CHART_VERSION) already exists;\
	fi

.PHONY: helm-push-via-cpd
helm-push-via-cpd:
	@echo ❌ target $@ is DEPRECATED - please update your Makefiles.; false

.PHONY: helm-push-artifactory-install
helm-push-artifactory-install:
	@echo ❌ target $@ is DEPRECATED - please update your Makefiles.; false

.PHONY: helm-push-via-plugin
helm-push-via-plugin:
	@echo ❌ target $@ is DEPRECATED - please update your Makefiles.; false

.PHONY: helm-setup-ci
helm-setup-ci:
	@echo 💬 checking / installing helm version $(HELM_VERSION)
	$(HELM_BINARY) version --short | grep -q $(HELM_VERSION) || \
		curl -s -L -o - $(HELM_TGZ) | tar -xz --strip-components=1 -C $(CI_BIN) linux-amd64/helm
	# if helm 2 is detected, run helm init
	@echo $(HELM_VERSION) | grep -Eq "^v2" && \
		$(HELM_BINARY) init --stable-repo-url "https://charts.helm.sh/stable" --client-only || true

.PHONY: helm-commit-deps
## Commit (and push) updated helm deps
helm-commit-deps:
	@echo 💬 commit and push changes to $(GIT_REMOTE_NAME) $(GIT_BRANCH_NAME)
	git diff --exit-code --name-status HEAD || \
		(git commit -m 'chore: $(UPSTREAM_CHART):$(UPSTREAM_VERSION) update chart deps' && \
		git push $(GIT_REMOTE_NAME) $(GIT_BRANCH_NAME))

.PHONY: helm-pin-dependency-in-downstream
## Update and deploy the deps
helm-pin-dependency-in-downstream:
	@echo 💬 updating downstream repo $(REPO_NAME) to pin $(CHART_NAME):$(CHART_VERSION)
	rm -rf $(REPO_NAME)
	git clone git@github.com:confluentinc/$(REPO_NAME).git $(REPO_NAME)
	$(MAKE) $(MAKE_ARGS) -C $(REPO_NAME) helm-pin-dependency-from-upstream helm-commit-deps \
		UPSTREAM_CHART=$(CHART_NAME) \
		UPSTREAM_VERSION=$(CHART_VERSION)
	@echo 💬 Successfully updated repo $(REPO_NAME) and pinned $(CHART_NAME):$(CHART_VERSION)

.PHONY: $(HELM_DOWNSTREAM_CHARTS)
## Update the downstream chart; pin the new chart version $(CHART_NAME):$(CHART_VERSION) as a dependency in $(HELM_DOWNSTREAM_CHARTS)
$(HELM_DOWNSTREAM_CHARTS):
ifeq ($(HOTFIX),true)
	@echo "💬 Skipping bumping downstream helm chart deps $@ on hotfix branch"
else ifeq ($(BUMP),major)
	@echo "💬 Skipping bumping downstream helm chart deps $@ with major version bump"
else
	@for i in $$(seq 1 3); do \
		echo "💬 Attempt to update downstream helm chart: $$i"; \
		$(MAKE) $(MAKE_ARGS) helm-pin-dependency-in-downstream REPO_NAME=$@ && break; \
	done
endif

.PHONY: test-helm-commands
## Test important helm commands, e.g. to validate a new helm version
test-helm-commands:
	$(MAKE) $(MAKE_ARGS) helm-update-repo
	$(MAKE) $(MAKE_ARGS) helm-install-deps
	$(MAKE) $(MAKE_ARGS) helm-update-floating-deps
	$(MAKE) $(MAKE_ARGS) helm-lint
	$(MAKE) $(MAKE_ARGS) helm-deploy-local
	$(MAKE) $(MAKE_ARGS) helm-clean
	$(MAKE) $(MAKE_ARGS) helm-package
