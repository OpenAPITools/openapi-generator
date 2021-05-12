# Addresses to halyard services
HALYARD_DEPLOYER_ADDRESS ?= halyard-deployer.prod.halyard.confluent.cloud:9090
HALYARD_RELEASE_ADDRESS ?= halyard-release.prod.halyard.confluent.cloud:9090
HALYARD_RENDERER_ADDRESS ?= halyard-renderer.prod.halyard.confluent.cloud:9090

# Determine which halyard services to auto bump source version
# List of halyard service files, default all.  All environments in these files will be bumped
HALYARD_SERVICE_FILES ?= $(wildcard .halyard/*.yaml)
# List of halyard service files with environments, defaults none.
# NOTE: This disables HALYARD_SERVICE_FILES, it's either full auto or full manual.
# NOTE: Apply always applies all files in HALYARD_SERVICE_FILES since it won't create new env
#       versions if there's nothing changed.
# Format: .halyard/service.yaml=env1 .halyard/service.yaml=env2 etc.
HALYARD_SERVICE_FILES_ENVS ?=
# Version to set source version to, defaults to current clean version without a v.
HALYARD_SOURCE_VERSION ?= $(BUMPED_CLEAN_VERSION)
# List of service/environments to automatically install on release, defaults none.
# Format: service=env service=env2 service2=env
HALYARD_INSTALL_SERVICE_ENVS ?=

# Only create a tmpdir on CI
ifeq ($(CI),true)
# we need ?= to allow overridding HAL_TMPDIR for CPD gating
HAL_TMPDIR ?= $(shell mktemp -d 2>/dev/null || mktemp -d -t 'halyard')
else
HAL_TMPDIR ?= $(PWD)
endif
# we need := for immediate assignment rather than deferred.
HAL_TMPDIR := $(HAL_TMPDIR)

# setup halctl cmd
HALYARD_VERSION ?= latest
HALCTL_ARGS ?=
HALYARD_IMAGE ?= confluent-docker.jfrog.io/confluentinc/halyard:$(HALYARD_VERSION)
_halctl_opts := --deployer-address $(HALYARD_DEPLOYER_ADDRESS)
_halctl_opts += --release-address $(HALYARD_RELEASE_ADDRESS)
_halctl_opts += --renderer-address $(HALYARD_RENDERER_ADDRESS)
_halctl_opts += $(HALCTL_ARGS)
_halctl_docker_opts := --user $(shell id -u):$(shell id -g) --rm -t
_halctl_docker_opts += -v $(PWD):/work -v $(HOME)/.halctl:/.halctl -w /work
ifeq ($(CI),true)
_halctl_docker_opts += -v $(HAL_TMPDIR):$(HAL_TMPDIR)
_halctl_docker_opts += --env-file ~/.halyard_secrets
endif
HALCTL ?= docker run $(_halctl_docker_opts) $(HALYARD_IMAGE) $(_halctl_opts)

INIT_CI_TARGETS += halyard-cache-image
RELEASE_PRECOMMIT += halyard-set-source-version
RELEASE_POSTCOMMIT += halyard-apply-services halyard-install-services

.PHONY: show-halyard
## Show Halyard Variables
show-halyard:
	@echo "HALYARD_SERVICE_FILES:        $(HALYARD_SERVICE_FILES)"
	@echo "HALYARD_SERVICE_FILES_ENVS:   $(HALYARD_SERVICE_FILES_ENVS)"
	@echo "HALYARD_INSTALL_SERVICE_ENVS: $(HALYARD_INSTALL_SERVICE_ENVS)"
	@echo "HALYARD_SOURCE_VERSION:       $(HALYARD_SOURCE_VERSION)"
	@echo "HALCTL:                       $(HALCTL)"

# target for caching the halyard docker image on semaphore
.PHONY: halyard-cache-image
halyard-cache-image:
	cache restore halyard-image
	test ! -f halyard-image.tgz || docker load -i halyard-image.tgz
	mv halyard-image.tgz halyard-image-prev.tgz || echo dummy > halyard-image-prev.tgz
	docker pull $(HALYARD_IMAGE)
	docker save $(HALYARD_IMAGE) | gzip --no-name > halyard-image.tgz
	cmp halyard-image-prev.tgz halyard-image.tgz || cache delete halyard-image; cache store halyard-image halyard-image.tgz
	rm -f halyard-image*.tgz

$(HOME)/.halctl:
	mkdir $(HOME)/.halctl

.PHONY: halctl
## Run halctl in the halyard docker image
halctl: $(HOME)/.halctl
	@$(HALCTL) $(HALCTL_ARGS)

.PHONY: halyard-set-source-version
ifeq ($(HALYARD_SERVICE_FILES_ENVS),)
halyard-set-source-version: $(HALYARD_SERVICE_FILES:%=set.%)
else
halyard-set-source-version: $(HALYARD_SERVICE_FILES_ENVS:%=set.%)
endif

.PHONY: $(HALYARD_SERVICE_FILES:%=set.%)
$(HALYARD_SERVICE_FILES:%=set.%): $(HOME)/.halctl
	$(HALCTL) release set-file-version -v $(HALYARD_SOURCE_VERSION) -f $(@:set.%=%)
	git add $(@:set.%=%)

.PHONY: $(HALYARD_SERVICE_FILES_ENVS:%=set.%)
$(HALYARD_SERVICE_FILES_ENVS:%=set.%): $(HOME)/.halctl
	@$(eval fpath := $(word 1,$(subst =, ,$@)))
	@$(eval env := $(word 2,$(subst =, ,$@)))
	$(HALCTL) release set-file-version -v $(HALYARD_SOURCE_VERSION) -f $(fpath) -e $(env)
	git add $(fpath)

.PHONY: halyard-apply-services
halyard-apply-services: $(HALYARD_SERVICE_FILES:%=apply.%)

.PHONY: $(HALYARD_SERVICE_FILES:%=apply.%)
$(HALYARD_SERVICE_FILES:%=apply.%): $(HOME)/.halctl
	$(HALCTL) release apply -f $(@:apply.%=%) --output-dir $(HAL_TMPDIR)

cc-releases:
	git clone git@github.com:confluentinc/cc-releases.git

.PHONY: update-cc-releases
update-cc-releases:
	git -C cc-releases checkout master
	git -C cc-releases pull

commit-cc-releases:
	git -C cc-releases diff --exit-code --cached --name-status || \
	(git -C cc-releases commit -m "chore: auto update" && \
	git -C cc-releases push)
	rm -rf cc-releases

.PHONY: halyard-list-service-version
halyard-list-service-version: $(HALYARD_INSTALL_SERVICE_ENVS:%=list.%)

# Retrieve the current running halyard version, for the service/env specified in 'HALYARD_INSTALL_SERVICE_ENVS'.
# The service source version is deteremined by 'git describe --contains', and the retrieved halyard version is saved into $(HAL_TMPDIR)/$(svc)/$(env)
# This target can be used together with halyard-install-services to install service version corresponding to a specific commit.
# E.g. `HALYARD_INSTALL_SERVICE_ENVS=cc-pipeline-service=stag make halyard-list-service-version halyard-install-services` during CI will install the
# current in-release cc-pipeline-service version onto stag environment
.PHONY: $(HALYARD_INSTALL_SERVICE_ENVS:%=list.%)
$(HALYARD_INSTALL_SERVICE_ENVS:%=list.%): $(HOME)/.halctl
	$(eval svc := $(word 1,$(subst =, ,$(@:list.%=%))))
	$(eval env := $(word 2,$(subst =, ,$(@:list.%=%))))
	$(eval src_ver := $(shell git rev-parse --is-inside-work-tree > /dev/null && git describe --contains | grep '^v[0-9]\+.[0-9]\+.[0-9]\+\(~[0-9]+\)\?$$' | cut -d'~' -f1 | cut -c 2-) )
	@echo "Found source version: $(src_ver)"
	@[[ ! -z "$(src_ver)" ]] || exit 1
	$(eval halyard_ver := $(shell set -o pipefail && $(HALCTL) release service env ver list $(svc) $(env) | grep $(src_ver) | tr -s ' ' | cut -d ' ' -f 2 | tail -1))
	@echo "Found halyard version: $(halyard_ver)"
	@[[ ! -z "$(halyard_ver)" ]] || exit 1
	@mkdir -p $(HAL_TMPDIR)/$(svc)
	echo $(halyard_ver) >> $(HAL_TMPDIR)/$(svc)/$(env)

.PHONY: halyard-wait-service-version
halyard-wait-service-version: $(HALYARD_INSTALL_SERVICE_ENVS:%=wait.%)

# Wait for the source version to be installed, for the service/env specified in 'HALYARD_INSTALL_SERVICE_ENVS'.
# The service source version is deteremined by 'git describe --contains', representing the new version tag commited after a successful 'release-ci'
# If the source version is identified, it periodically queries halyard to wait for the version being succesffully installed on all relevant k8s clusters,
# otherwise it fails after a timeout, currently default to 20 iteration with 30 seconds interval, equals to 10 mins.
# E.g. `HALYARD_INSTALL_SERVICE_ENVS=cc-pipeline-service=devel make halyard-wait-service-version` will wait for current in-release verion to be installed on devel.
.PHONY: $(HALYARD_INSTALL_SERVICE_ENVS:%=wait.%)
$(HALYARD_INSTALL_SERVICE_ENVS:%=wait.%): $(HOME)/.halctl
	$(eval svc := $(word 1,$(subst =, ,$(@:wait.%=%))))
	$(eval env := $(word 2,$(subst =, ,$(@:wait.%=%))))
	$(eval src_ver := $(shell git rev-parse --is-inside-work-tree > /dev/null && git describe --contains | grep '^v[0-9]\+.[0-9]\+.[0-9]\+~1$$' | cut -d'~' -f1 | cut -c 2-) )
	@echo "Found source version: $(src_ver)"
	@[[ ! -z "$(src_ver)" ]] || exit 1
	$(eval halyard_ver := $(shell set -o pipefail && $(HALCTL) release service env ver list $(svc) $(env) | grep $(src_ver) | tr -s ' ' | cut -d ' ' -f 2 | tail -1))
	@echo "Found halyard version: $(halyard_ver)"
	@[[ ! -z "$(halyard_ver)" ]] || exit 1
	@LOOP_COUNT=0; LOOP_TOTAL=20; LOOP_INTERVAL=30; \
	until [ $$LOOP_COUNT -eq $$LOOP_TOTAL ] || (echo "waiting version $(src_ver) to be installed..." && $(HALCTL) release service env ver get $(svc) $(env) $(halyard_ver) -o json | jq -r .installStatus[].status 2>&1 | grep -v DONE | wc -l | tr -d ' ' | grep '^0$$'); \
	do $(HALCTL) release service env ver get $(svc) $(env) $(halyard_ver) -o json | jq -r .installStatus; (( LOOP_COUNT=LOOP_COUNT+1 )); [ $$LOOP_COUNT -lt $$LOOP_TOTAL ] && echo "still waiting..." && sleep $$LOOP_INTERVAL; done; \
	[ $$LOOP_COUNT -lt $$LOOP_TOTAL ] || (echo "Time out on waiting for version to be installed..." && exit 1)
	@echo "Source version $(src_ver) is installed"

.PHONY: halyard-install-services
halyard-install-services: cc-releases update-cc-releases $(HALYARD_INSTALL_SERVICE_ENVS:%=install.%) commit-cc-releases

.PHONY: $(HALYARD_INSTALL_SERVICE_ENVS:%=install.%)
$(HALYARD_INSTALL_SERVICE_ENVS:%=install.%): $(HOME)/.halctl
	$(eval svc := $(word 1,$(subst =, ,$(@:install.%=%))))
	$(eval env := $(word 2,$(subst =, ,$(@:install.%=%))))
	$(eval ver := $(shell cat $(HAL_TMPDIR)/$(svc)/$(env)))
	$(HALCTL) release set-file-install-version -v $(ver) -f cc-releases/services/$(svc)/$(env).yaml
	git -C cc-releases add services/$(svc)/$(env).yaml

.PHONY: halyard-cpd-publish-dirty
halyard-cpd-publish-dirty: halyard-set-source-version halyard-apply-services

.PHONY: halyard-cpd-install-dirty
halyard-cpd-install-dirty: $(HALYARD_INSTALL_SERVICE_ENVS:%=cpd.%)

.PHONY: $(HALYARD_INSTALL_SERVICE_ENVS:%=cpd.%)
$(HALYARD_INSTALL_SERVICE_ENVS:%=cpd.%): $(HOME)/.halctl
	$(eval svc := $(word 1,$(subst =, ,$(@:cpd.%=%))))
	$(eval env := $(word 2,$(subst =, ,$(@:cpd.%=%))))
	$(eval ver := $(shell cat $(HAL_TMPDIR)/$(svc)/$(env)))
	@echo "## Installing service in CPD cluster with halyard";
	$(HALCTL) release service environment version install $(svc) $(env) $(ver) -c $(CPD_CLUSTER_ID)
	@echo "## Forcing halyard-agent to reconcile";
	kubectl -n halyard-agent patch agent deployer-agent --type=json  -p '[{"op":"remove","path":"/status"}]'
	sleep 30
	@echo "## Checking service status in halyard";
	$(HALCTL) release service environment version get $(svc) $(env) $(ver)
