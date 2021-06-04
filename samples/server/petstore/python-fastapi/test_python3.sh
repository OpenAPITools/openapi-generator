#!/bin/bash

### install dependencies
pip3 install -r requirements.txt

### run the tests
PYTHONPATH=src pytest tests
