# cc-deployer is for auto-deploying to devel via obs-helm deployer
# Usage: include this file and set DEPLOYER_DOWNSTREAM_DEPS=obs-helm

RELEASE_NAME ?= $(SERVICE_NAME)

RELEASE_VERSION ?= $(VERSION_NO_V)
RELEASE_CLUSTER ?= k8saas-eks-mothership-devel

RELEASE_MAKE_TARGETS += $(DEPLOYER_DOWNSTREAM_DEPS)

.PHONY: show-deployer
## Show deployer variables
show-deployer:
	@echo "RELEASE_NAME: $(RELEASE_NAME)"
	@echo "RELEASE_VERSION: $(RELEASE_VERSION)"
	@echo "RELEASE_CLUSTER: $(RELEASE_CLUSTER)"
	@echo "DEPLOYER_DOWNSTREAM_DEPS: $(DEPLOYER_DOWNSTREAM_DEPS)"

.PHONY: deployer-update-deps
## Update UPSTREAM_RELEASE/VERSION dependency in the UPSTREAM_CLUSTER
deployer-update-deps:
	./deployer update $(UPSTREAM_CLUSTER) $(UPSTREAM_RELEASE) $(UPSTREAM_VERSION)
	# Semaphore ~/.aws/credentials must have a [default] AWS Eng credentials and a [semaphoreci] AWS umbrella profile
	AWS_PROFILE=semaphoreci ./deployer deploy -u caas-team -k id_rsa $(UPSTREAM_CLUSTER) $(UPSTREAM_RELEASE)

.PHONY: deployer-commit-deps
## Commit (and push) updated deployer deps
deployer-commit-deps:
	git diff --exit-code --name-status || \
		(git add clusters/$(UPSTREAM_CLUSTER)/releases.json && \
		git commit -m 'chore: $(UPSTREAM_RELEASE):v$(UPSTREAM_VERSION) update release deps' && \
		git push $(GIT_REMOTE_NAME) $(GIT_BRANCH_NAME))

.PHONY: deployer-deploy-deps
## Update and deploy the deps
deployer-deploy-deps:
	@echo "Start to deploy $(RELEASE_NAME):$(RELEASE_VERSION) to $(RELEASE_CLUSTER) via $(REPO_NAME)"
	rm -rf $(REPO_NAME)
	git clone git@github.com:confluentinc/$(REPO_NAME).git $(REPO_NAME)
	$(MAKE) -C $(REPO_NAME) install-sops deployer-update-deps deployer-commit-deps \
		UPSTREAM_RELEASE=$(RELEASE_NAME) \
		UPSTREAM_VERSION=$(RELEASE_VERSION) \
		UPSTREAM_CLUSTER=$(RELEASE_CLUSTER)
	@echo "Successfully deployed $(RELEASE_NAME):$(RELEASE_VERSION) to $(RELEASE_CLUSTER) via $(REPO_NAME)"

.PHONY: $(DEPLOYER_DOWNSTREAM_DEPS)
$(DEPLOYER_DOWNSTREAM_DEPS):
ifeq ($(HOTFIX),true)
	@echo "Skipping bumping downstream deployer deps $@ on hotfix branch"
else ifeq ($(BUMP),major)
	@echo "Skipping bumping downstream deployer deps $@ with major version bump"
else
	@for i in $$(seq 1 3); do \
		echo "Attempt to bump downstream deployer deps: $$i"; \
		$(MAKE) deployer-deploy-deps REPO_NAME=$@ && break; \
	done
endif
