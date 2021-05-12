RELEASE_POSTCOMMIT += cc-scheduler-plugins/$(CC_PLUGIN_PACKAGE)/$(CC_PLUGIN_SERVICE)_docker.go

CC_PLUGIN_PACKAGE ?=
ifeq ($(CC_PLUGIN_PACKAGE),)
$(error CC_PLUGIN_PACKAGE must be set)
endif

CC_PLUGIN_SERVICE ?=
ifeq ($(CC_PLUGIN_SERVICE),)
$(error CC_PLUGIN_SERVICE must be set)
endif

# Default to the the bumped version
CC_PLUGIN_IMAGE_VERSION ?= $(BUMPED_VERSION)

# Default to the docker image name
CC_PLUGIN_IMAGE_NAME ?= $(IMAGE_NAME)

# Template for constant overrides
define CC_PLUGIN_DOCKER=
package $(CC_PLUGIN_PACKAGE)

const (
	$(CC_PLUGIN_SERVICE)ImageName = "$(CC_PLUGIN_IMAGE_NAME)"
	$(CC_PLUGIN_SERVICE)ImageTag  = "$(CC_PLUGIN_IMAGE_VERSION)"
)
endef
export CC_PLUGIN_DOCKER

.PHONY: show-cc-scheduler-plugin
## Show cc-scheduler-plugin info
show-cc-scheduler:
	@echo "package: $(CC_PLUGIN_PACKAGE)"
	@echo "service: $(CC_PLUGIN_SERVICE)"
	@echo "image name: $(CC_PLUGIN_IMAGE_NAME)"
	@echo "image tag: $(CC_PLUGIN_IMAGE_VERSION)"
	@echo "cc-scheduler-plugins/$(CC_PLUGIN_PACKAGE)/$(CC_PLUGIN_SERVICE)_docker.go:"
	@echo -e "$${CC_PLUGIN_DOCKER}"

cc-scheduler-plugins/$(CC_PLUGIN_PACKAGE)/$(CC_PLUGIN_SERVICE)_docker.go: cc-scheduler-plugins
	@echo -e "$${CC_PLUGIN_DOCKER}" > $@
	git -C $< add $@
	git -C $< commit -m 'chore($(CC_PLUGIN_PACKAGE)): update $(CC_PLUGIN_SERVICE) image defaults'
	git -C $< push origin master
	rm -rf $<

.PHONY: cc-scheduler-plugins
cc-scheduler-plugins:
	(test -d $@ && rm -rf $@) || true
	git clone git@github.com:confluentinc/$@.git $@
