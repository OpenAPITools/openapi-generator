BUILD_TARGETS += mvn-install
CLEAN_TARGETS += mvn-clean
TEST_TARGETS += mvn-verify
RELEASE_PRECOMMIT += mvn-set-bumped-version
RELEASE_POSTCOMMIT += mvn-deploy

MAVEN_ARGS ?= --no-transfer-progress
ifeq ($(CI),true)
MAVEN_ARGS += --batch-mode
# Put local maven repo inside CI_BIN to leverage caching done in cc-semaphore.mk
MAVEN_OPTS += -Dmaven.repo.local=$(CI_BIN)/m2 -Dmaven.artifact.threads=10

# enable CI profile for spotbugs, test-coverage, and dependency analysis
MAVEN_PROFILES += jenkins
endif

# Use predefine MVN or local `mvnw` if present in the repo, else fallback to globally installed `mvn`
ifeq ($(wildcard $(MVN)),)
MVN := $(GIT_ROOT)/mvnw
endif
ifeq ($(wildcard $(MVN)),)
MVN := mvn
endif
MVN += $(MAVEN_ARGS)
MVN += $(MAVEN_OPTS)
MVN += $(foreach profile,$(MAVEN_PROFILES),-P$(profile))

MAVEN_SKIP_CHECKS=-DskipTests=true \
        -Dcheckstyle.skip=true \
        -Dspotbugs.skip=true \
        -Djacoco.skip=true \
        -Ddependency-check.skip=true

MAVEN_INSTALL_ARGS := --update-snapshots $(MAVEN_SKIP_CHECKS) install

.PHONY: mvn-install
mvn-install:
ifneq ($(MAVEN_INSTALL_PROFILES),)
	$(MVN) $(foreach profile,$(MAVEN_INSTALL_PROFILES),-P$(profile)) $(MAVEN_INSTALL_ARGS)
else
	$(MVN) $(MAVEN_INSTALL_ARGS)
endif

ifeq ($(CI),true)
mvn-install: mvn-set-bumped-version
endif

.PHONY: mvn-verify
mvn-verify:
	$(MVN) verify

.PHONY: mvn-clean
mvn-clean:
	$(MVN) clean

# Requires a <distributionManagement> section in your pom.xml
# Alternatively, set <maven.deploy.skip>true</maven.deploy.skip> in your pom.xml to skip deployment
.PHONY: mvn-deploy
mvn-deploy:
	$(MVN) deploy $(MAVEN_SKIP_CHECKS)

# Set the version in pom.xml to the bumped version
.PHONY: mvn-set-bumped-version
mvn-set-bumped-version:
	$(MVN) versions:set \
		-DnewVersion=$(BUMPED_CLEAN_VERSION) \
		-DgenerateBackupPoms=false
	$(GIT) add --verbose pom.xml '*/pom.xml'

# Other projects have a superstitious dependency on docker-pull-base here
# instead of letting `docker build` just automatically pull the base image.
# If we start seeing build issues on MacOS we can resurrect this dependency.
# https://confluent.slack.com/archives/C6KU9M23A/p1559867903037100
#
#BASE_IMAGE := confluent-docker.jfrog.io/confluentinc/cc-base
#BASE_VERSION := v3.2.0
#mvn-docker-package: docker-pull-base
.PHONY: mvn-docker-package
mvn-docker-package:
	$(MVN) package \
	        $(MAVEN_SKIP_CHECKS) \
		--activate-profiles docker \
		-Ddocker.tag=$(IMAGE_VERSION) \
		-Ddocker.registry=$(DOCKER_REPO)/ \
		-DGIT_COMMIT=$(shell git describe --always --dirty) \
		-DBUILD_NUMBER=$(BUILD_NUMBER)
	docker tag $(DOCKER_REPO)/confluentinc/$(IMAGE_NAME):$(IMAGE_VERSION) \
		confluentinc/$(IMAGE_NAME):$(IMAGE_VERSION)

ifeq ($(CI),true)
	docker image save confluentinc/$(IMAGE_NAME):$(IMAGE_VERSION) | gzip | \
		artifact push project /dev/stdin -d docker/$(BRANCH_NAME)/$(IMAGE_VERSION).tgz --force
endif

.PHONY: show-maven
show-maven:
	@echo "MVN:                     $(MVN)"
	@echo "MAVEN_OPTS:              $(MAVEN_OPTS)"
	@echo "MAVEN_INSTALL_PROFILES:  $(MAVEN_INSTALL_PROFILES)"
