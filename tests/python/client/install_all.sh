#!/bin/bash

PREVIOUS_DIR=$(pwd)
REQUIREMENTS_DIR=$(dirname "$0")
(cd "$REQUIREMENTS_DIR" && pip install -r requirements.txt && cd "$PREVIOUS_DIR")
