_empty :=
_space := $(_empty) $(empty)

# Gets added after the version
VERSION_POST ?=

# Auto bump by default
BUMP ?= auto
# If on master branch bump the minor by default
ifeq ($(RELEASE_BRANCH),$(MASTER_BRANCH))
DEFAULT_BUMP ?= minor
# Else bump the patch by default
else
DEFAULT_BUMP ?= patch
endif

# Enable tacking on a timestamp to the versions.
TS ?=

# For hotfix PRs convert checkout to full clone to make sure git describe can execute properly
ifneq ($(SEMAPHORE_GIT_BRANCH),$(_empty))
ifneq ($(shell echo $(SEMAPHORE_GIT_BRANCH) | grep -E '^v[0-9]+.[0-9]+.x'),$(_empty))
$(shell git fetch --unshallow)
endif
endif

# make sure master branch version bump always use the global latest version
ifeq ($(BRANCH_NAME),$(MASTER_BRANCH))
# cut -d -f equals cut --delimiter= --field, short version is compatible with mac
LATEST_VERSION := $(shell git ls-remote --tags --refs --sort="v:refname" | \
tail -n1 | tr -d " \t\n\r" | cut -d'/' -f3)
VERSION_SUFFIX := $(shell git describe --tags --always --dirty | cut -d'-' -s -f2,3,4)
ifeq ($(VERSION_SUFFIX),$(_empty))
VERSION := $(LATEST_VERSION)
else
VERSION := $(LATEST_VERSION)-$(VERSION_SUFFIX)
endif
else
VERSION := $(shell git describe --tags --always --dirty)
endif

ifneq (,$(findstring dirty,$(VERSION)))
ifeq ($(TS),)
VERSION := $(VERSION)-$(USER)
else
VERSION := $(VERSION)-$(USER)-$(shell date +%s)
endif
endif
CLEAN_VERSION := $(shell echo $(VERSION) | grep -Eo '([0-9]+\.){2}[0-9]+')
VERSION_NO_V := $(shell echo $(VERSION) | sed 's,^v,,' )

CI_SKIP ?= [ci skip]

ifeq ($(CLEAN_VERSION),$(_empty))
CLEAN_VERSION := 0.0.0
else
GIT_MESSAGES := $(shell git log --pretty='%s' v$(CLEAN_VERSION)...HEAD 2>/dev/null | tr '\n' ' ')
endif

# If auto bump enabled, search git messages for bump hash
ifeq ($(BUMP),auto)
_auto_bump_msg := \(auto\)
ifneq (,$(findstring \#major,$(GIT_MESSAGES)))
BUMP := major
else ifneq (,$(findstring \#minor,$(GIT_MESSAGES)))
BUMP := minor
else ifneq (,$(findstring \#patch,$(GIT_MESSAGES)))
BUMP := patch
else
BUMP := $(DEFAULT_BUMP)
endif
endif

# Figure out what the next version should be
split_version := $(subst .,$(_space),$(CLEAN_VERSION))
ifeq ($(BUMP),major)
bump := $(shell expr $(word 1,$(split_version)) + 1)
BUMPED_CLEAN_VERSION := $(bump).0.0
else ifeq ($(BUMP),minor)
bump := $(shell expr $(word 2,$(split_version)) + 1)
BUMPED_CLEAN_VERSION := $(word 1,$(split_version)).$(bump).0
else ifeq ($(BUMP),patch)
bump := $(shell expr $(word 3,$(split_version)) + 1)
BUMPED_CLEAN_VERSION := $(word 1,$(split_version)).$(word 2,$(split_version)).$(bump)
endif

BUMPED_CLEAN_VERSION := $(BUMPED_CLEAN_VERSION)$(VERSION_POST)
BUMPED_VERSION := v$(BUMPED_CLEAN_VERSION)

RELEASE_SVG := <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="94" height="20"><linearGradient id="b" x2="0" y2="100%"><stop offset="0" stop-color="\#bbb" stop-opacity=".1"/><stop offset="1" stop-opacity=".1"/></linearGradient><clipPath id="a"><rect width="94" height="20" rx="3" fill="\#fff"/></clipPath><g clip-path="url(\#a)"><path fill="\#555" d="M0 0h49v20H0z"/><path fill="\#007ec6" d="M49 0h45v20H49z"/><path fill="url(\#b)" d="M0 0h94v20H0z"/></g><g fill="\#fff" text-anchor="middle" font-family="DejaVu Sans,Verdana,Geneva,sans-serif" font-size="110"><text x="255" y="150" fill="\#010101" fill-opacity=".3" transform="scale(.1)" textLength="390">release</text><text x="255" y="140" transform="scale(.1)" textLength="390">release</text><text x="705" y="150" fill="\#010101" fill-opacity=".3" transform="scale(.1)" textLength="350">$(BUMPED_VERSION)</text><text x="705" y="140" transform="scale(.1)" textLength="350">$(BUMPED_VERSION)</text></g> </svg>

.PHONY: show-version
## Show version variables
show-version:
	@echo version: $(VERSION)
	@echo version no v: $(VERSION_NO_V)
	@echo clean version: $(CLEAN_VERSION)
	@echo version bump: $(BUMP) $(_auto_bump_msg)
	@echo bumped version: $(BUMPED_VERSION)
	@echo bumped clean version: $(BUMPED_CLEAN_VERSION)
	@echo version post append: $(VERSION_POST)
	@echo 'release svg: $(RELEASE_SVG)'
ifeq ($(CI_TEST),true)
	@echo 'This is under CI test environment, any git push opertaion will be skipped'
endif

.PHONY: tag-release
tag-release:
	$(GIT) tag $(BUMPED_VERSION)
	$(GIT) push $(GIT_REMOTE_NAME) $(BUMPED_VERSION)
	# version bump commit may fail if there are queueing builds
	$(GIT) push $(GIT_REMOTE_NAME) $(RELEASE_BRANCH) || true

.PHONY: get-release-image
get-release-image:
	echo '$(RELEASE_SVG)' > release.svg
	git add release.svg

.PHONY: commit-release
commit-release:
	git diff --exit-code --cached --name-status || \
	git commit -m "chore: $(BUMP) version bump $(BUMPED_VERSION) $(CI_SKIP)"
