INIT_CI_TARGETS += python-deps
TEST_TARGETS += python-lint python-test

PYTHON_VERSION ?= 3.7
PIPENV_VERSION ?= 2018.11.26

PYTEST ?= pipenv run python -m pytest
PYTEST_ARGS ?=
PYTEST_IGNORE_DIR = mk-include
# You can also use pytest.ini to ignore if you overwrite pytest cmds
PYTEST_ARGS += --ignore=$(PYTEST_IGNORE_DIR)

DOCKER_BUILD_OPTIONS += --build-arg pipenv_version=$(PIPENV_VERSION)

 # Ignore any active virtualenv and use the pipenv managed virtualenv instead
PIPENV_IGNORE_VIRTUALENVS ?= 1
export PIPENV_IGNORE_VIRTUALENVS

.PHONY: python-resources
python-resources:
	cp mk-include/resources/.flake8 ./.flake8
	cp mk-include/resources/.style.yapf ./.style.yapf
	cp mk-include/resources/.yapfignore ./.yapfignore
	cp mk-include/resources/.pylintrc ./.pylintrc

.PHONY: python-deps
## Setup the python env with dependencies
python-deps:
	pip install pipenv==$(PIPENV_VERSION)
	pipenv install --python $(PYTHON_VERSION) yapf flake8 isort pytest pylint --dev
	pipenv install --dev

.PHONY: python-lint
## Lint the python code against project standards
python-lint:
	pipenv run yapf -rd .
	pipenv run flake8 .
	find . -iname '*.py' | xargs pipenv run pylint  # to lint a dir it must be a python module; instead run file-by-file
	@# this crazy looking stuff is just to make isort print errors and exit 1; it only has "strict mode" as githook
	pipenv run isort --diff -rc . | grep -v 'Skipped' > /tmp/isort.log ; \
		cat /tmp/isort.log ; test ! -s /tmp/isort.log ; code=$$? ; rm -r /tmp/isort.log ; exit $$code

.PHONY: python-fmt
## Format the python code to follow the project standards
python-fmt:
	pipenv run yapf -i ./*.py

.PHONY: python-test
## Run all python tests (pytest)
python-test:
	$(PYTEST) $(PYTEST_ARGS)
