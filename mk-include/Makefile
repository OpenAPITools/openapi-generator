include ./cc-begin.mk
include ./cc-vault.mk
include ./cc-semver.mk
include ./cc-testbreak.mk
include ./cc-end.mk

DOCKER_GO_TEST_LOCATION := tests/go-docker-build-test/
MK_INCLUDE_AUTO_UPDATE_TEST_LOCATION := tests/mk-include-auto-update-test/
MAVEN_DOCKER_BUILD_TEST := tests/maven-docker-build-test/
MK_INCLUDE := mk-include/
UPDATE_MK_INCLUDE := false
CC_MK_INCLUDE := cc-mk-include

.PHONY: copy-mk-include-go-docker-build-test
copy-mk-include-go-docker-build-test:
	find . -name '*.mk' | cpio -pdm "${DOCKER_GO_TEST_LOCATION}""${MK_INCLUDE}"
	cp -R bin/. "${DOCKER_GO_TEST_LOCATION}""${MK_INCLUDE}""bin"
	cp .gitignore "${DOCKER_GO_TEST_LOCATION}"

.PHONY: copy-mk-include-mk-include-auto-update-test
copy-mk-include-mk-include-auto-update-test:
	find . -name '*.mk' | cpio -pdm "${MK_INCLUDE_AUTO_UPDATE_TEST_LOCATION}""${MK_INCLUDE}"
	cp -R bin "${MK_INCLUDE_AUTO_UPDATE_TEST_LOCATION}""${MK_INCLUDE}"
	cp .gitignore "${MK_INCLUDE_AUTO_UPDATE_TEST_LOCATION}"

.PHONY: copy-mk-include-maven-docker-build-test
copy-mk-include-maven-docker-build-test:
	find . -name '*.mk' | cpio -pdm "${MAVEN_DOCKER_BUILD_TEST}""${MK_INCLUDE}"
	cp -R bin "${MAVEN_DOCKER_BUILD_TEST}""${MK_INCLUDE}"

.PHONY: copy-parent-mk-include
copy-parent-mk-include:
	find . -name '*.mk' | cpio -pdm "${MK_INCLUDE}"
	cp -R bin/. "${MK_INCLUDE}""bin"

.PHONY: upload-binary
upload-binary:
	cd .. ;\
	tar --exclude='$(CC_MK_INCLUDE)/.git' --exclude='$(CC_MK_INCLUDE)/.semaphore' \
	--exclude='$(CC_MK_INCLUDE)/tests' --exclude='$(CC_MK_INCLUDE)/.DS_Store' \
	--exclude='$(CC_MK_INCLUDE)/mk-include' --exclude='$(CC_MK_INCLUDE)/ci-bin' \
	-zcvf $(CC_MK_INCLUDE)_$(BUMPED_VERSION).tar.gz $(CC_MK_INCLUDE) ;\
	aws s3 cp $(CC_MK_INCLUDE)_$(BUMPED_VERSION).tar.gz s3://$(CC_MK_INCLUDE) ;\
	cp $(CC_MK_INCLUDE)_$(BUMPED_VERSION).tar.gz $(CC_MK_INCLUDE)_master.tar.gz ;\
	aws s3 cp $(CC_MK_INCLUDE)_master.tar.gz s3://$(CC_MK_INCLUDE) ;

.PHONY: verify-version
verify-version:
	@[[ "$(VERSION)" =~ ^v[0-9]+\.[0-9]+\.([0-9]+$$|[0-9]+-[0-9]+-[a-zA-Z0-9]+$$) ]] && echo "version format verified"
