# Downstream TF Consumer Format: (replace : with ^)
#	GIT_URI//TF_FILE_PATH//MODULE_NAME
# Exampe: git@github.com^confluentinc/cc-terraform-module-mothership.git//internal-resources/main.tf//gateway-service
#
# Note that this is a list so list all downstream consumers here
#
_noop :=
_space := $(_noop) $(_noop)

INIT_CI_TARGETS += $(BIN_PATH)/terraform
TEST_TARGETS += tf-fmt
RELEASE_PRECOMMIT += set-tf-bumped-version
RELEASE_MAKE_TARGETS += bump-downstream-tf-consumers
CLEAN_TARGETS += clean-terraform

# Terraform Variables
TF_VERSION ?= 0.11.13
MODULE_NAME ?= $(IMAGE_NAME)
define BUMPED_IMAGE_VERSION_OVERRIDE
{ "variable": { "image_version": { "default": "$(BUMPED_VERSION)" } } }
endef

DOWNSTREAM_TF_CONSUMERS ?=

.PHONY: show-downstream-tf-consumers
show-downstream-tf-consumers:
	@echo 'Downstream TF Consumers:'
	@$(foreach consumer,$(DOWNSTREAM_TF_CONSUMERS),echo "  $(subst ^,:,$(consumer))";)

.PHONY: bump-downstream-tf-consumers
bump-downstream-tf-consumers: $(DOWNSTREAM_TF_CONSUMERS)

# Only bump down stream consumers on the master branch, too complex to deal with hotfixes in tf automatically
$(DOWNSTREAM_TF_CONSUMERS):
ifeq ($(BRANCH_NAME),master)
	$(eval consumer := $(subst ^,:,$@))
	$(eval split_consumer := $(subst //,$(_space),$(consumer)))
	$(eval git_uri := $(word 1, $(split_consumer)))
	$(eval tf_path := $(word 2, $(split_consumer)))
	$(eval tf_module := $(word 3, $(split_consumer)))
	$(eval repo_name := $(basename $(notdir $(git_uri))))
	git clone $(git_uri)
	vim -Ec '/module "$(tf_module)" {/|/source\s\+=/|s/?ref=v[0-9.]\+/?ref=v$(CLEAN_VERSION)/|p|x' "./$(repo_name)/$(tf_path)"
	git -C "./$(repo_name)" add "$(tf_path)"
	git -C "./$(repo_name)" diff --exit-code --cached --name-status || \
		(git -C "./$(repo_name)" commit -m "TF Module Bump: $(tf_path)//$(tf_module) to $(CLEAN_VERSION)" && \
		 git -C "./$(repo_name)" push origin master)
	rm -rf $(repo_name)
else
	true
endif

.PHONY: deploy-terraform
deploy-terraform:
	cd terraform/deployments/minikube && \
		terraform init && \
		terraform apply -var "image_version=$(VERSION)"

.PHONY: clean-terraform
clean-terraform:
	rm -rf terraform/deployments/minikube/*.tfstate* terraform/deployments/minikube/.terraform*

.PHONY: set-tf-bumped-version
set-tf-bumped-version:
	test -d terraform \
		&& (echo '$(BUMPED_IMAGE_VERSION_OVERRIDE)' > terraform/modules/$(MODULE_NAME)/image_version_override.tf.json && \
			git add terraform/modules/$(MODULE_NAME)/image_version_override.tf.json) || true

.PHONY: tf-fmt
tf-fmt:
	find . -name '*.tf' -not -path "./.semaphore*" -not -path "*.terraform*" | $(XARGS) -L1 terraform fmt -check -diff

$(BIN_PATH)/terraform:
	@curl -Ssq "https://releases.hashicorp.com/terraform/$(TF_VERSION)/terraform_$(TF_VERSION)_linux_amd64.zip" | sudo zcat > $(BIN_PATH)/terraform ; sudo chmod +x $(BIN_PATH)/terraform

terraform: $(BIN_PATH)/terraform
