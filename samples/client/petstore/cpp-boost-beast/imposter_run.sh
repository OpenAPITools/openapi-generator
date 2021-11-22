#!/usr/bin/env bash

set -u
set -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "${SCRIPT_DIR}"

SPEC_FILE_LINK="${SCRIPT_DIR}/imposter_config/petstore.yaml"

if [[ ! -f "${SPEC_FILE_LINK}" ]]; then
  SPEC_FILE_ORIGINAL="$(pwd)/../../../../modules/openapi-generator/src/test/resources/3_0/petstore.yaml"
  echo "Create hard link form '${SPEC_FILE_ORIGINAL}' to '${SPEC_FILE_LINK}'"
  ln "${SPEC_FILE_ORIGINAL}" "${SPEC_FILE_LINK}"
fi

imposter up -p 8080 ./imposter_config

#rm -f "${SPEC_FILE_LINK}" || true
