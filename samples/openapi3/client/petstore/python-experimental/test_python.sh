#!/bin/bash

REQUIREMENTS_FILE=dev-requirements.txt
REQUIREMENTS_OUT=dev-requirements.txt.log
SETUP_OUT=*.egg-info
VENV=venv
DEACTIVE=false

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

### set virtualenv
if [ -z "$VENVV" ]; then
		python3 -m venv $VENV
		source $VENV/bin/activate
    DEACTIVE=true
fi

### install dependencies
pip install -r $REQUIREMENTS_FILE | tee -a $REQUIREMENTS_OUT
### locally install the package, needed for pycharm problem checking
pip install -e .

### run tests
tox || exit 1

### static analysis of code
#flake8 --show-source petstore_api/

### deactivate virtualenv
#if [ $DEACTIVE == true ]; then
#    deactivate
#fi
