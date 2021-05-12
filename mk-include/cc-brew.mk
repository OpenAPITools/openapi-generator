BREW_FORMULA ?= changeme
BREW_GITHUB_REPO ?= confluentinc/homebrew-internal
BREW_VERSION ?= $(CLEAN_VERSION)
BREW_PKG ?=

ifeq ($(shell uname),Darwin)
SED ?= gsed
else
SED ?= sed
endif

.PHONY: show-brew
## Show brew info
show-brew:
	@echo "Formula: $(BREW_FORMULA)"
	@echo "Repo:    $(BREW_GITHUB_REPO)"
	@echo "Version: $(BREW_VERSION)"
	@echo "Package: $(BREW_PKG)"

.PHONY: bump-formula-version
## Bump the brew formula to the current version
bump-formula-version:
	$(eval chksum := $(shell openssl sha256 -r $(BREW_PKG) | cut -d ' ' -f 1))
	git clone git@github.com:$(BREW_GITHUB_REPO) brew-tap
	$(SED) -i 's/formula_version = \"[0-9.]\+\"/formula_version = "$(BREW_VERSION)"/' brew-tap/Formula/$(BREW_FORMULA).rb
	$(SED) -i "s/sha256 '.\+'/sha256 '$(chksum)'/" brew-tap/Formula/$(BREW_FORMULA).rb
	git -C $(PWD)/brew-tap add Formula/$(BREW_FORMULA).rb
	git -C $(PWD)/brew-tap diff --exit-code --cached --name-status || \
		(git -C $(PWD)/brew-tap commit -m "Bumping $(BREW_FORMULA) to $(BREW_VERSION)" && \
		 git -C $(PWD)/brew-tap push origin master)
	rm -rf brew-tap
