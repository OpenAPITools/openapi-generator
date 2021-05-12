SERVICE_NAME := test-service
IMAGE_NAME := cc-$(SERVICE_NAME)
CHART_NAME := cc-$(SERVICE_NAME)

CI_TEST := true
UPDATE_MK_INCLUDE := true

include ./mk-include/cc-begin.mk
include ./mk-include/cc-semver.mk
include ./mk-include/cc-docker.mk
include ./mk-include/cc-end.mk
include ./mk-include/cc-cpd.mk
include ./mk-include/cc-testbreak.mk
include ./mk-include/cc-vault.mk

.PHONY: mk-include-auto-update-test
mk-include-auto-update-test:
	python3 -m venv mk-include-auto-update-test
	source mk-include-auto-update-test/bin/activate && \
	pip3 install \
	    pytest=="$(PYTEST_VERSION)" \
	    structlog=="${STRUCTLOG_VERSION}" && \
	pytest test.py -s --log-level="${PYTEST_LOG_LEVEL}"
