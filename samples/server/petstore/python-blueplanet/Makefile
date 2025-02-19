HIDE ?= @
PACKAGE := openapi_server
RELEASE := 1.0.0
VENV := env
PYPI ?= 'https://pypi.blueplanet.com/simple'

PWD ?= $(shell pwd)

# Solution
VENDOR ?= path_to_your_repo
SOLUTION_RELEASE ?= $(shell solmaker version solution/fig.yml)
SOLUTION_NAME ?= $(shell solmaker name solution/fig.yml)
SOLUTION_IMAGE := $(VENDOR)/solution-$(SOLUTION_NAME)

DOCKER_BUILD := docker build
DOCKER_IMAGE ?= $(VENDOR)/$(PACKAGE)
REGISTRY ?= registry.blueplanet.com
REMOTE_SERVER ?= bpadmin@10.10.10.10

SOLUTION := $(REGISTRY).$(VENDOR).$(SOLUTION_NAME):$(SOLUTION_RELEASE)

.PHONY: app solution update upload-solution

app: check-env
	$(DOCKER_BUILD) --build-arg GLTOKEN=${GLTOKEN} -t $(REGISTRY)/$(DOCKER_IMAGE):$(RELEASE) -f app/Dockerfile ./app

remote:
	docker save $(REGISTRY)/$(DOCKER_IMAGE):$(RELEASE) | bzip2 | pv | ssh $(REMOTE_SERVER) 'bunzip2 | docker load'
	docker save $(REGISTRY)/$(SOLUTION_IMAGE):$(SOLUTION_RELEASE) | bzip2 | pv | ssh $(REMOTE_SERVER) 'bunzip2 | docker load'

push:
	docker push $(REGISTRY)/$(DOCKER_IMAGE):$(RELEASE)
	docker push $(REGISTRY)/$(SOLUTION_IMAGE):$(SOLUTION_RELEASE)

solution: check-env
	$(docker rmi $(REGISTRY)/$(SOLUTION_IMAGE):$(SOLUTION_RELEASE) && true)
	solmaker build solution/fig.yml --vendor=$(VENDOR) --tag=$(SOLUTION_RELEASE)

upload-solution:
	docker save $(REGISTRY)/$(SOLUTION_IMAGE):$(SOLUTION_RELEASE) | bzip2 | pv | ssh $(REMOTE_SERVER) 'bunzip2 | docker load'

update: app solution
	-ssh $(REMOTE_SERVER) 'solman "solution_purge -y $(SOLUTION)"'
	docker save $(REGISTRY)/$(DOCKER_IMAGE):$(RELEASE) | bzip2 | pv | ssh $(REMOTE_SERVER) 'bunzip2 | docker load'
	docker save $(REGISTRY)/$(SOLUTION_IMAGE):$(SOLUTION_RELEASE) | bzip2 | pv | ssh $(REMOTE_SERVER) 'bunzip2 | docker load'
	-ssh $(REMOTE_SERVER) 'solman "solution_deploy $(SOLUTION)"'

check-env:
ifndef GLTOKEN
    $(error GLTOKEN is undefined)
endif
