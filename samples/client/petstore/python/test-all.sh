#!/bin/sh

REQUIREMENTS_FILE=dev-requirements.txt
REQUIREMENTS_OUT=dev-requirements.txt.log
SETUP_OUT=*.egg-info
VENV=.venv

### set virtualenv
if [ -z "$VIRTUAL_ENV" ]; then
		virtualenv $VENV --no-site-packages
		source $VENV/bin/activate
fi

### install dependencies
pip install -r $REQUIREMENTS_FILE | tee -a $REQUIREMENTS_OUT
python setup.py develop

### run tests
tox
