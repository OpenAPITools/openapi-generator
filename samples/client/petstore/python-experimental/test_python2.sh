#!/bin/bash

REQUIREMENTS_FILE=dev-requirements.txt
REQUIREMENTS_OUT=dev-requirements.txt.log
SETUP_OUT=*.egg-info
VENV=venv
DEACTIVE=false

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
PYTHONPATH="$(which python)"

### set virtualenv
if [ -z "$VIRTUAL_ENV" ]; then
		virtualenv $VENV --python=$PYTHONPATH --no-site-packages --always-copy
		source $VENV/bin/activate
    DEACTIVE=true
fi

### install dependencies
pip install -r $REQUIREMENTS_FILE | tee -a $REQUIREMENTS_OUT
pip install -r test-requirements.txt
python setup.py develop

### run tests
nosetests || exit 1

### static analysis of code
flake8 --show-source petstore_api/

### deactivate virtualenv
#if [ $DEACTIVE == true ]; then
#    deactivate
#fi

