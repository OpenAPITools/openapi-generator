#!/bin/bash

BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BRANCH_NAME=${BRANCH_NAME:-$(git rev-parse --abbrev-ref HEAD)}
EXPECTED_K8S=${EXPECTED_K8S:-k8s-sz-a1}
REVISION=${SEMAPHORE_GIT_SHA:-${REVISION}}

if [ "${CI}" != "true" ]; then
  echo "This script is designed to be run by CI only"
  exit 1
fi

# Promotes successful system-tests to the next environment
function promote() {
  ENV=$1
  NEXT_ENV=$2

  if [ -n "${ENV}" -a -n "${NEXT_ENV}" ]; then
    echo "Marking tests stable to run in ${NEXT_ENV}."
    git branch -f "${NEXT_ENV}" "${ENV}"
    git push -f origin "${NEXT_ENV}"
  else
    echo "ENV or NEXT_ENV not set, nothing to update..."
    exit 0
  fi
}

# Runs the system tests in an environment and promote to the next if they pass
function run_tests() {
  ENV=$1
  NEXT_ENV=$2

  case "${SEMAPHORE_TRIGGER_SOURCE}" in
    # This is a change to the tests, a change in configuration, or a promotion after tests succeeded in an earlier env
    'push'|'manual'|'api')
      PROMOTE=true
      ;;
    # This is a scheduled build for a specific environment branch
    'scheduler')
      PROMOTE=false
      ;;
    *)
      echo "Unknown Trigger Source... Not doing anything"
      exit 1
      ;;
  esac

  if [[ $BRANCH_NAME == pull-request-* ]]; then
    PR_ID=$(echo $BRANCH_NAME | cut -d '-' -f 3)
  fi

  echo "Running test directory ${GO_TEST_PACKAGE_ARGS}"
  if "${BASEDIR}/run_tests.sh" -test-package "$GO_TEST_PACKAGE_ARGS" -env "${ENV}" -commit "${REVISION}" -pr-id "${PR_ID}" -cloud aws -region us-west-2 -expected-k8s "${EXPECTED_K8S}" -ssh-key ~/.ssh/id_rsa -ssh-user caas-team; then
    echo "Tests succeeded!"
    if [ "${PROMOTE}" = "true" ]; then
      promote "${ENV}" "${NEXT_ENV}"
    fi
    exit 0
  else
    echo "Tests failed!"
    exit 1
  fi
}

echo "Branch: ${BRANCH_NAME}"
echo "Trigger: ${SEMAPHORE_TRIGGER_SOURCE}"
echo "Connect tests envs: ${CONNECT_SKIP_TESTS}, ${CONNECT_SKIP_SOURCE_TESTS}, ${CONNECT_SKIP_SINK_TESTS}"

case "${BRANCH_NAME}" in
  'master')
    if make vet lint-go; then
      # automatically push code changes to devel branch
      promote master devel

      if [ "${ENABLE_MASTER_TO_STAG_PROMOTION}" = "true" ]; then
        # optionally push code changes to stag branch
        promote master stag
      fi
      if [ "${ENABLE_MASTER_TO_CPD_PROMOTION}" = "true" ]; then
        # optionally push code changes to cpd branch
        promote master cpd
      fi
    else
      echo 'make vet lint-go failed'
      exit 1
    fi
    ;;

  'devel')
    run_tests devel
    ;;

  'stag')
    run_tests stag
    ;;

  'prod')
    echo "Production system testing via CI not supported yet!"
    exit 1
    ;;

  *)
    # Feature branches
    if make vet lint-go; then
      # Note: disable running system tests in devel env to avoid affecting
      # the potentially parallel tests run for devel branch.
      # Reason: our current tests have the limitation of sharing same test
      # accounts, clusters, quota, etc.
      # If the team does have some tests safe to run in parallel, the team
      # can opt in to run those selected tests on PR branches.
      if [ -n "${TESTS_TO_RUN_ON_PR_BRANCH}" ]; then
        TESTS_TO_RUN=${TESTS_TO_RUN_ON_PR_BRANCH} run_tests devel
      fi
    else
      echo 'make vet lint-go failed'
      exit 1
    fi
    ;;
esac
