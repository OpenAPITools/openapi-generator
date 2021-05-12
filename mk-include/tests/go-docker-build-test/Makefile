DOCKER_SSH_MOUNT := 1

SERVICE_NAME := test-service
IMAGE_NAME := cc-$(SERVICE_NAME)
CHART_NAME := cc_test_service
GO_BINS := github.com/confluentinc/cc-test-service/cmd/server=test-service

DEPLOYER_DOWNSTREAM_DEPS := obs-helm

RELEASE_PRECOMMIT := pre-release-check

CI_TEST := true

include ./mk-include/cc-begin.mk
include ./mk-include/cc-vault.mk
include ./mk-include/cc-semver.mk
include ./mk-include/cc-go.mk
include ./mk-include/cc-docker.mk
include ./mk-include/cc-cpd.mk
include ./mk-include/cc-helm.mk
include ./mk-include/cc-deployer.mk
include ./mk-include/cc-testbreak.mk
include ./mk-include/cc-end.mk

GO_TEST_ARGS += -p=1

.PHONY: go-docker-build-test
go-docker-build-test:
	python3 -m venv go-docker-build-test
	source go-docker-build-test/bin/activate && \
	pip3 install \
	    pytest=="$(PYTEST_VERSION)" \
	    structlog=="${STRUCTLOG_VERSION}" && \
	pytest test.py -s --log-level="${PYTEST_LOG_LEVEL}"
