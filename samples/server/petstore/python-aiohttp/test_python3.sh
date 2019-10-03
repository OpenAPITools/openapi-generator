#!/bin/bash

REQUIREMENTS_FILE=requirements.txt
TEST_REQUIREMENTS_FILE=test-requirements.txt
REQUIREMENTS_OUT=requirements.txt.log
SETUP_OUT=*.egg-info
VENV=.venv
DEACTIVE=false

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

### set virtualenv
if [ -z "$VIRTUAL_ENV" ]; then
	virtualenv $VENV --no-site-packages --always-copy --python python3
		source $VENV/bin/activate
    DEACTIVE=true
fi

### install dependencies
pip install -r $REQUIREMENTS_FILE -r $TEST_REQUIREMENTS_FILE | tee -a $REQUIREMENTS_OUT

### run tests
pytest || exit 1

### static analysis of code
flake8 --show-source petstore_api/

### deactivate virtualenv
if [ $DEACTIVE == true ]; then
    deactivate
fi
