VAULT_VERSION ?= v1.4.0
VAULT_VERSION_NO_V := $(shell echo $(VAULT_VERSION) | sed -e 's/^v//')

INIT_CI_TARGETS += install-vault
INIT_CI_TARGETS += vault-bash-functions

VAULT_INSTALLED_VERSION := $(shell $(BIN_PATH)/vault -version 2>/dev/null | head -n 1 | awk '{ print $$2 }')
VAULT_DL_LOC := https://vault-zipfile-public-cache.s3-us-west-2.amazonaws.com/vault_$(VAULT_VERSION_NO_V)_$(HOST_OS)_amd64.zip

.PHONY: show-vault
show-vault:
	@echo "BIN_PATH: $(BIN_PATH)"
	@echo "VAULT_VERSION: $(VAULT_VERSION)"
	@echo "VAULT_VERSION_NO_V: $(VAULT_VERSION_NO_V)"
	@echo "VAULT_INSTALLED_VERSION: $(VAULT_INSTALLED_VERSION)"
	@echo "HOST_OS: $(HOST_OS)"

.PHONY: install-vault
install-vault:
ifneq ($(VAULT_VERSION),$(VAULT_INSTALLED_VERSION))
	@echo "Installing Hashicorp Vault $(VAULT_VERSION) from $(VAULT_DL_LOC)"
	@wget --timeout=20 --tries=15 --retry-connrefused -q -O /tmp/vault.zip $(VAULT_DL_LOC)
	@echo "Unzipping received /tmp/vault.zip" && cd /tmp && unzip vault.zip
	@mv -f /tmp/vault $(BIN_PATH)/vault
	@chmod +x $(BIN_PATH)/vault
	@echo "Placed vault in $(BIN_PATH)/vault"
endif

.PHONY: vault-bash-functions
vault-bash-functions:
	@cp mk-include/bin/vault-setup $(BIN_PATH)
	@cp mk-include/bin/vault-sem-get-secret $(BIN_PATH)
