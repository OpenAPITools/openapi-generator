REQUIREMENTS_FILE=dev-requirements.txt
REQUIREMENTS_OUT=dev-requirements.txt.log
SETUP_OUT=*.egg-info

virtualenv:
ifndef VIRTUAL_ENV
		$(error Must be run inside of a virtualenv\
     http://docs.python-guide.org/en/latest/dev/virtualenvs/)
endif

setup: virtualenv $(SETUP_OUT)

$(SETUP_OUT): setup.py setup.cfg
		python setup.py develop
		touch $(SETUP_OUT)

$(REQUIREMENTS_OUT): $(REQUIREMENTS_FILE)
		pip install -r $(REQUIREMENTS_FILE) | tee -a $(REQUIREMENTS_OUT)
		python setup.py develop

clean:
		rm -rf $(REQUIREMENTS_OUT)
		rm -rf $(SETUP_OUT)
		rm -rf .tox
		rm -rf .coverage
		find . -name "*.py[oc]" -delete
		find . -name "__pycache__" -delete

requirements: setup $(REQUIREMENTS_OUT)

test: clean requirements
		nosetests

test-all: clean requirements
		tox


