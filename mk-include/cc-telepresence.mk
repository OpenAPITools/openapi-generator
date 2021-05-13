GO_SWAP_NAMESPACE ?= cc-system
GO_SWAP_DEPLOYMENT ?= $(MODULE_NAME)
GO_SWAP_COMMAND ?=
GO_SWAP_ARGS ?=
GO_SWAP_DEBUG ?= false
GO_SWAP_ALSO_PROXY ?=
GO_SWAP_EXTRA_OPTS ?=

.PHONY: show-go-swap
show-go-swap:
	@echo "CCLOUD_ENV: $(CCLOUD_ENV)"
	@echo "GO_SWAP_NAMESPACE: $(GO_SWAP_NAMESPACE)"
	@echo "GO_SWAP_DEPLOYMENT: $(GO_SWAP_DEPLOYMENT)"
	@echo "GO_SWAP_COMMAND: $(GO_SWAP_COMMAND)"
	@echo "GO_SWAP_ARGS: $(GO_SWAP_ARGS)"
	@echo "GO_SWAP_DEBUG: $(GO_SWAP_DEBUG)"
	@echo "GO_SWAP_ALSO_PROXY: $(GO_SWAP_ALSO_PROXY)"
	@echo "GO_SWAP_EXTRA_OPTS: $(GO_SWAP_EXTRA_OPTS)"

.PHONY: go-swap-local
go-swap-local:
	$(MAKE) build-go
ifdef GO_SWAP_ARGS
	$(eval args := $(GO_SWAP_ARGS))
else
	$(eval args := $(shell bash -c "kubectl -n $(GO_SWAP_NAMESPACE) get deploy/$(GO_SWAP_DEPLOYMENT) -o json | jq '.spec.template.spec.containers[].args[]' -r"))
endif
ifdef GO_SWAP_COMMAND
	$(eval command := $(GO_SWAP_COMMAND))
else
	$(eval command := $(shell bash -c "kubectl -n $(GO_SWAP_NAMESPACE) get deploy/$(GO_SWAP_DEPLOYMENT) -o json | jq '.spec.template.spec.containers[].command[]?' -r"))
	$(eval split := $(subst =, ,$(GO_BINS)))
	$(eval command := $(if $(command),$(command),$(word 2,$(split))))
endif
ifeq ($(GO_SWAP_DEBUG),true)
	telepresence \
	--namespace $(GO_SWAP_NAMESPACE) \
	--swap-deployment $(GO_SWAP_DEPLOYMENT) \
	$(GO_SWAP_EXTRA_OPTS) $(addprefix --also-proxy , $(GO_SWAP_ALSO_PROXY)) \
	--run $(MK_INCLUDE_BIN)/dlv-with-args.sh $(GO_OUTDIR)/$(command) '$(args)'
else
	telepresence \
	--namespace $(GO_SWAP_NAMESPACE) \
	--swap-deployment $(GO_SWAP_DEPLOYMENT) \
	$(GO_SWAP_EXTRA_OPTS) $(addprefix --also-proxy , $(GO_SWAP_ALSO_PROXY)) \
	--run $(GO_OUTDIR)/$(command) $(args)
endif
