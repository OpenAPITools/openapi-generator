PROTOC := $(BIN_PATH)/protoc
PROTOC_VERSION ?= 3.9.0
PROTOC_INSTALLED_VERSION := $(shell $(PROTOC) --version 2>/dev/null protoc | awk '{print $$2}')

uname := $(shell uname)
ifeq ($(uname),Darwin)
PROTOC_OS := osx
else ifeq ($(uname),Linux)
PROTOC_OS := linux
endif

PROTOC_ARCH := $(shell uname -m)

.PHONY: install-protoc
install-protoc:
ifneq ($(PROTOC_VERSION),$(PROTOC_INSTALLED_VERSION))
	mkdir -p /tmp/protoc && \
	curl -L -o /tmp/protoc/protoc.zip https://github.com/protocolbuffers/protobuf/releases/download/v$(PROTOC_VERSION)/protoc-$(PROTOC_VERSION)-$(PROTOC_OS)-$(PROTOC_ARCH).zip && \
	cd /tmp/protoc && \
	unzip -j protoc.zip -d $(BIN_PATH) bin/protoc && \
	unzip protoc.zip -d $(BIN_PATH) include/* && \
	rm -rf /tmp/protoc
endif


.PHONY: clean-protoc
clean-protoc:
	rm -rf $(BIN_PATH)/protoc
	rm -rf $(BIN_PATH)/../include/google
