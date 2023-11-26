#!/bin/bash

set -e # Fail if any command fails
set -x # Print executed commands

SCRIPT_DIR=$(dirname "$0")
ROOT_DIR=$(dirname "$0")/../../..
# Note: These cannot run in one process, because some tests have the same import paths
pytest -v "$ROOT_DIR/samples/client/echo_api/python"
pytest -v "$ROOT_DIR/samples/client/echo_api/python-disallowAdditionalPropertiesIfNotPresent-true"
pytest -v "$ROOT_DIR/samples/openapi3/client/petstore/python"
pytest -v "$ROOT_DIR/samples/openapi3/client/petstore/python-aiohttp"
pytest -v "$SCRIPT_DIR"
