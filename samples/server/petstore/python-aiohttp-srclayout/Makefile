 #!/bin/bash

REQUIREMENTS_OUT=requirements.txt.log
VENV=.venv

clean:
		rm -rf $(REQUIREMENTS_OUT)
		rm -rf $(VENV)
		rm -rf .pytest_cache
		rm -rf .coverage
		find . -name "*.py[oc]" -delete
		find . -name "__pycache__" -delete

venv: 
		python -m venv $(VENV)

test: clean venv
		bash ./test_python3.sh

test-all: clean venv
		bash ./test_python3.sh
