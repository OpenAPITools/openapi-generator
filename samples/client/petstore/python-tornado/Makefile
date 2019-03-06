 #!/bin/bash

REQUIREMENTS_FILE=test-requirements.txt
SETUP_OUT=*.egg-info
VENV=.venv

clean:
		rm -rf $(SETUP_OUT)
		rm -rf $(VENV)
		rm -rf .tox
		rm -rf .coverage
		find . -name "*.py[oc]" -delete
		find . -name "__pycache__" -delete

test-all: clean
		bash ./test_python2_and_3.sh
