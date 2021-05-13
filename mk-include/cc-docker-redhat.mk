RELEASE_POSTCOMMIT += push-docker-redhat
BUILD_TARGETS += build-docker-redhat

# if RHEL_VERSION not defined suffix with tag rhel7 will be appended
ifndef RHEL_VERSION
RHEL_TAG_NAME := $(BUILD_TAG)-rhel7
else
RHEL_TAG_NAME := $(BUILD_TAG)-$(RHEL_VERSION)
endif

# redhat release version scheme based on redhat certification process
minor_version := $(subst .,$(_space),$(VERSION_NO_V))
RHEL_RELEASE_NUMBER := $(shell expr $(word 2,$(minor_version)))

.PHONY: build-docker-redhat
build-docker-redhat: .netrc .ssh
	docker build -f Dockerfile.rhel7 $(DOCKER_BUILD_OPTIONS) --no-cache --build-arg version=$(IMAGE_VERSION) --build-arg release=$(RHEL_RELEASE_NUMBER) -t $(RHEL_TAG_NAME) .
	rm -rf .netrc .ssh

.PHONY: tag-docker-redhat
tag-docker-redhat:
	@echo 'create docker tag $(IMAGE_VERSION)'
	docker tag $(RHEL_TAG_NAME) $(DOCKER_REPO)/$(RHEL_TAG_NAME)

.PHONY: push-docker-redhat
push-docker-redhat: tag-docker-redhat
	@echo 'push $(IMAGE_VERSION) to $(DOCKER_REPO)'
	docker push $(DOCKER_REPO)/$(RHEL_TAG_NAME)
