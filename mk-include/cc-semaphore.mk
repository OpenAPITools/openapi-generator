ifeq ($(SEMAPHORE_2),true)
# In Semaphore 2, the cache must be manually managed.
# References:
#   https://docs.semaphoreci.com/article/68-caching-dependencies
#   https://docs.semaphoreci.com/article/54-toolbox-reference#cache

INIT_CI_TARGETS += ci-bin-sem-cache-restore
EPILOGUE_TARGETS += ci-bin-sem-cache-store
ifeq ($(SEMAPHORE_GIT_PR_BRANCH),)
    CACHE_KEY = ci-bin_$(SEMAPHORE_GIT_BRANCH)
else
    CACHE_KEY = ci-bin_$(SEMAPHORE_GIT_PR_BRANCH)
endif

.PHONY: ci-bin-sem-cache-store
ci-bin-sem-cache-store:
	@echo "Storing semaphore caches"
	cache delete $(CACHE_KEY) \
		&& cache store $(CACHE_KEY) ci-bin
	cache store gocache $(GOPATH)/pkg/mod

.PHONY: ci-bin-sem-cache-restore
ci-bin-sem-cache-restore:
	@echo "Restoring semaphore caches"
	cache restore $(CACHE_KEY),ci-bin_master,ci-bin
	cache restore gocache

.PHONY: ci-bin-sem-cache-delete
ci-bin-sem-cache-delete:
	@echo "Deleting semaphore caches"
	cache delete $(CACHE_KEY)
endif
