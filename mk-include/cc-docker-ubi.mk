RELEASE_POSTCOMMIT += push-docker-ubi
BUILD_TARGETS += build-docker-ubi

# if UBI_VERSION not defined suffix with tag ubi8 will be appended
ifndef UBI_VERSION
RHEL_UBI_TAG_NAME := $(BUILD_TAG)-ubi8
else
RHEL_UBI_TAG_NAME := $(BUILD_TAG)-$(UBI_VERSION)
endif
# RedHat release version scheme based on RedHat ubi
minor_version := $(subst .,$(_space),$(VERSION_NO_V))
RHEL_UBI_RELEASE_NUMBER := $(shell expr $(word 2,$(minor_version)))

.PHONY: build-docker-ubi
build-docker-ubi:
	cp $(HOME)/.netrc .netrc
	cp -R $(HOME)/.ssh/. .ssh
	docker build -f Dockerfile.ubi $(DOCKER_BUILD_OPTIONS) --no-cache --build-arg version=$(IMAGE_VERSION) --build-arg release=$(RHEL_UBI_RELEASE_NUMBER) -t $(RHEL_UBI_TAG_NAME) .
	rm -rf .netrc .ssh
ifeq ($(CI),true)
	docker image save $(RHEL_UBI_TAG_NAME) | gzip | \
		artifact push project /dev/stdin -d docker/$(BRANCH_NAME)/$(IMAGE_VERSION)-ubi.tgz --force
endif


.PHONY: restore-docker-version-ubi
restore-docker-version-ubi:
ifeq ($(CI),true)
	artifact pull project docker/$(BRANCH_NAME)/$(IMAGE_VERSION)-ubi.tgz -d /dev/stdout --force | \
		gunzip | docker image load
endif

.PHONY: tag-docker-ubi
tag-docker-ubi:
	@echo 'create docker tag $(IMAGE_VERSION)'
	docker tag $(RHEL_UBI_TAG_NAME) $(DOCKER_REPO)/$(RHEL_UBI_TAG_NAME)

.PHONY: push-docker-ubi
push-docker-ubi: restore-docker-version-ubi tag-docker-ubi
	@echo 'push $(IMAGE_VERSION) to $(DOCKER_REPO)'
	docker push $(DOCKER_REPO)/$(RHEL_UBI_TAG_NAME)
