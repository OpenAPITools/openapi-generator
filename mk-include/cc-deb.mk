DEB_PKG_NAME ?= $(SERVICE_NAME)
DEB_OUTDIR ?= deb
RELEASE_MAKE_TARGETS += publish-deb-docker

# Check if version includes a - which means it's not a released version
ifeq ($(findstring -,$(VERSION_NO_V)), "")
APT_SUITE := testing
else
APT_SUITE := stable
endif

.aws:
	cp -r ~/.aws/. .aws

.PHONY: publish-deb-docker
## Build and publish deb inside packaging docker container
publish-deb-docker: .netrc .aws
ifneq ($(DOCKER_SSH_MOUNT),)
	@echo "Adding keys to agent for ssh support"
	@ssh-add || ssh-add -l | grep -q '@confluent.io' || (echo "Unable to add default identities. Manually add keys to the agent using ssh-add."; exit 1)
endif
ifeq ($(GO_USE_VENDOR),-mod=vendor)
ifneq ($(CI),true)
	@$(MAKE) deps
endif
endif
	docker build -f Dockerfile.deb-package $(DOCKER_BUILD_OPTIONS) --build-arg version=$(VERSION_NO_V) -t build-deb .
	docker image rm -f build-deb
	rm -rf .aws

.PHONY: build-deb
## Build deb package using fpm
build-deb:
	cd mk-include/packaging && \
		gem install bundler && \
		bundle install && \
		gem install specific_install && \
		gem specific_install https://github.com/confluentinc/fpm.git && \
		gem uninstall specific_install && \
		fpm \
			-s dir \
			-t deb \
			-n $(DEB_PKG_NAME) \
			-v $(VERSION_NO_V) \
			-p $(DEB_OUTDIR)/$(DEB_PKG_NAME)_$(VERSION_NO_V)_amd64.deb \
			$(BUILDROOT)=/

.PHONY: publish-deb
## Upload deb to s3://cloud-confluent-apt
publish-deb: build-deb
	cd mk-include/packaging && \
		deb-s3 upload \
			--arch amd64 \
			--suite $(APT_SUITE) \
			--lock \
			--fail-if-exists \
			--visibility private \
			--s3-region us-west-2 \
			--bucket cloud-confluent-apt \
			--preserve-versions \
			$(DEB_OUTDIR)/$(DEB_PKG_NAME)_$(VERSION_NO_V)_amd64.deb
