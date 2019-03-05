#!/bin/bash

REQUIREMENTS_FILE=dev-requirements.txt
REQUIREMENTS_OUT=dev-requirements.txt.log
SETUP_OUT=*.egg-info
VENV=.venv
DEACTIVE=false

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

### set virtualenv
# on mac machines with python3, virtualenv is a python3 utility
# so we need to find the python2 location and use it when making our VENV
PY_LOC=$(which python)
if [ -z "$VIRTUAL_ENV" ]; then
		virtualenv $VENV --python=$PY_LOC --no-site-packages --always-copy
		source $VENV/bin/activate
    DEACTIVE=true
fi

### install dependencies
pip install -r $REQUIREMENTS_FILE | tee -a $REQUIREMENTS_OUT
python setup.py develop

### run tests
nosetests || exit 1

### static analysis of code
flake8 --show-source petstore_api/

### deactivate virtualenv
#if [ $DEACTIVE == true ]; then
#    deactivate
#fi
