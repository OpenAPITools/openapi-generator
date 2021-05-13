LIBRDKAFKA_VERSION ?= 0.11.5

TMPDIR ?= $(SEMAPHORE_CACHE_DIR)
TMPDIR ?= /tmp

INIT_CI_TARGETS += install-librdkafka

ifeq ($(CI),true)
export LD_LIBRARY_PATH := /usr/local/lib/
endif

.PHONY: install-librdkafka
## Compile and install librdkafka from source, set version with LIBRDKAFKA_VERSION
install-librdkafka:
	git clone https://github.com/edenhill/librdkafka.git $(TMPDIR)/librdkafka
	git -C $(TMPDIR)/librdkafka checkout v$(LIBRDKAFKA_VERSION)
	cd $(TMPDIR)/librdkafka && ./configure
	$(MAKE) -C $(TMPDIR)/librdkafka
	sudo $(MAKE) -C $(TMPDIR)/librdkafka install
	rm -rf $(TMPDIR)/librdkafka
