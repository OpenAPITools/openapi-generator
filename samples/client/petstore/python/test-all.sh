#!/bin/bash

REQUIREMENTS_FILE=dev-requirements.txt
REQUIREMENTS_OUT=dev-requirements.txt.log
SETUP_OUT=*.egg-info
VENV=.venv
DEACTIVE=false

### set virtualenv
if [ -z "$VIRTUAL_ENV" ]; then
		virtualenv $VENV --no-site-packages --always-copy
		source $VENV/bin/activate
    DEACTIVE=true
fi

### install dependencies
pip install -r $REQUIREMENTS_FILE | tee -a $REQUIREMENTS_OUT
python setup.py develop

### run tests
tox

### deactivate virtualenv
if [ $DEACTIVE == true ]; then
    deactivate
fi
