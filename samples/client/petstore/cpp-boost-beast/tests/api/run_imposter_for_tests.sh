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
# NOTE: as of version 4.5.2, imposter default values have changed
# https://github.com/imposter-project/imposter-jvm-engine/commit/06abdf394ccb94d6d9b9c3b2e2eb424940386e20
imposter up -p 8080 ./imposter_config &
pid=$!

function terminate_test_server()
{
    local payload_exit_status="$?"
    trap - EXIT

    if kill -0 "${pid}" 2>/dev/null; then
      kill -TERM "${pid}" 2>/dev/null || true
      for (( shutdown_attempt = 0; shutdown_attempt < 20; shutdown_attempt++ )); do
        if ! kill -0 "${pid}" 2>/dev/null; then
          break
        fi
        sleep 0.25
      done

      if kill -0 "${pid}" 2>/dev/null; then
        kill -KILL "${pid}" 2>/dev/null || true
      fi
    fi

    wait "${pid}" 2>/dev/null || true
    exit "${payload_exit_status}"
}

trap terminate_test_server EXIT


echo Waiting for imposter
set -x
imposter_startup_deadline=$((SECONDS + 60))
while [[ "$(curl --max-time 5 -s -o /dev/null -w ''%{http_code}'' localhost:8080/_spec/)" != "200" ]]; do
  if (( SECONDS >= imposter_startup_deadline )); then
    echo "Timed out waiting for imposter to start" >&2
    exit 1
  fi
  sleep 5
done
set +x

echo Running tests: "$@"
set +e
"$@"
test_exit_status=$?

exit "${test_exit_status}"
