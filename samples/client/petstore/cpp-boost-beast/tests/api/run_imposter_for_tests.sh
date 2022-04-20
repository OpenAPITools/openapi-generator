#!/usr/bin/env bash
set -u
set -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "${SCRIPT_DIR}"

SPEC_FILE_LINK="${SCRIPT_DIR}/imposter_config/petstore.yaml"

if [[ ! -f "${SPEC_FILE_LINK}" ]]; then
  SPEC_FILE_ORIGINAL="$(pwd)/../../../../../../modules/openapi-generator/src/test/resources/3_0/petstore.yaml"
  echo "Create hard link form '${SPEC_FILE_ORIGINAL}' to '${SPEC_FILE_LINK}'"
  ln "${SPEC_FILE_ORIGINAL}" "${SPEC_FILE_LINK}"
fi


echo Starting imposter
imposter up -p 8080 ./imposter_config &
pid=$!

function kill_test_server()
{
    kill -9 $pid || true
}

trap kill_test_server EXIT ERR


echo Waiting for imposter
set -x
while [[ "$(curl -s -o /dev/null -w ''%{http_code}'' localhost:8080/_spec/)" != "200" ]]; do sleep 5; done
set +x
sleep 5

echo Running tests: "$@"
"$@"

imposter down
