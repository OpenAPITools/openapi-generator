BRANCH_NAME ?= $(SEMAPHORE_GIT_BRANCH)
JOB_NAME ?= $(SEMAPHORE_JOB_NAME)
SEMAPHORE_BUILD_NUMBER ?= $(SEMAPHORE_JOB_ID)
SEMAPHORE_JOB_URL ?= $(SEMAPHORE_ORGANIZATION_URL)/jobs/$(SEMAPHORE_JOB_ID)

# We don't want to report to testbreak for branches that aren't whitelisted.
TESTBREAK_REPORTING_BRANCHES ?= $(MASTER_BRANCH)

ifneq (,$(findstring $(BRANCH_NAME),$(TESTBREAK_REPORTING_BRANCHES)))
INIT_CI_TARGETS += testbreak-setup
endif

export BRANCH_NAME
export SEMAPHORE_BUILD_NUMBER
export SEMAPHORE_JOB_URL

.PHONY: testbreak-setup
testbreak-setup:
	sudo pip3 install confluent-ci-tools
	ci-timestamp

.PHONY: testbreak-after
testbreak-after:
ifneq (,$(findstring $(BRANCH_NAME),$(TESTBREAK_REPORTING_BRANCHES)))
	@echo "Reporting to testbreak at url: https://testbreak.confluent.io/kiosk/branch/$(BRANCH_NAME)/job_result/$(SEMAPHORE_PROJECT_NAME), branch \"$(BRANCH_NAME)\" is whitelisted."
	ci-kafka-event --build-log build.log --test-results "$(BUILD_DIR)/**/TEST*xml"  # report to testbreak
else
	@echo "Not reporting to testbreak, branch \"$(BRANCH_NAME)\" not whitelisted in [$(TESTBREAK_REPORTING_BRANCHES)]."
endif
