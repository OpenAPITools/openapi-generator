# Common make targets for using https://github.com/roboll/helmfile
# Helmfile is a tool for automated git-ops style deployments

HELMFILE_VERSION := v0.133.0
HELMDIFF_VERSION := 3.1.2
HELMFILE_URL := https://github.com/roboll/helmfile/releases/download

HELMFILES := $(strip $(shell find -type f -name helmfile.yaml -printf '%p '))

INIT_CI_TARGETS += helmfile-install-ci helmdiff-install-ci
TEST_TARGETS += helmfile-test

$(CI_BIN)/helmfile:
	curl -L -s -o - $(HELMFILE_URL)/$(HELMFILE_VERSION)/helmfile_linux_amd64 > $(CI_BIN)/helmfile \
		&& chmod a+x $(CI_BIN)/helmfile

.PHONY: helmfile-install-ci
helmfile-install-ci: $(CI_BIN)/helmfile
	 $(CI_BIN)/helmfile --version | grep '$(HELMFILE_VERSION)$$' || { rm -f $(CI_BIN)/helmfile && $(MAKE) $(CI_BIN)/helmfile ; }

.PHONY: helmdiff-install-ci
helmdiff-install-ci: helm-setup-ci
	helm plugin install https://github.com/databus23/helm-diff --version $(HELMDIFF_VERSION)

.PHONY: helmfile-test
helmfile-test: $(HELMFILES:%=helmfile-test.%)

# Run both `helmfile lint` and `helmfile template` since `helm lint` does not
# fail when required values are missing.
# Redirect `helmfile template` output to /dev/null to avoid leaking secrets in build logs
$(HELMFILES:%=helmfile-test.%):
	helmfile --file $(@:helmfile-test.%=%) template --skip-deps > /dev/null
	helmfile --file $(@:helmfile-test.%=%) lint --skip-deps

# You will typically need an aws-update-kubeconfig target in your project's Makefile and declare it as
# a prerequisite to helmfile-apply-ci.  For example:
# helmfile-apply: aws-update-kubeconfig
# aws-update-kubeconfig:
# 	aws eks update-kubeconfig --name k8s-mz-monitoring-eks--prod--796641d1f56d5fea --alias k8s-mz-monitoring-eks

.PHONY: helmfile-apply-ci
helmfile-apply: $(HELMFILES:%=helmfile-apply.%)

$(HELMFILES:%=helmfile-apply.%):
	helmfile --file $(@:helmfile-apply.%=%) apply
