#!/bin/bash

SCRIPT_NAME="$0"
SCRIPT_PATH="$(cd "$(dirname "$0")"; pwd)"

SSH_USER=${CAAS_USER:-$USER}
SSH_KEY=${HOME}/.ssh/caas-${SSH_USER}
GITHUB_KEY=${GITHUB_KEY:-${HOME}/.ssh/github}

CLOUD=${CLOUD:-aws}
REGION=${REGION:-us-west-2}

# Ensure bin path is setup
export PATH="$PATH:${PATH}/bin"

function usage() {
  cat <<USAGE
Usage: $0 -env ENV
Required:
  -env      Environment to test, ex: devel, stag (not production!)
Optional:
  -test-package      Select test package to run (e.g. ./test/paas/...)
  -cloud             Cloud in which to run tests
  -region            Region in which to run tests
  -expected-k8s      Kubernetes context in which clusters are scheduled (e.g., k8s-sz-a1)
  -commit            Commit in cc-system-tests to run, defaults to master
  -pr-id             Id of PR into confluentinc/cc-system-tests. If set, --commit value is ignored in favor of PR branch head.
  -ssh-key           Use this ssh key for Github and Bastion (for CI mostly)
  -ssh-user          Login with this user, not \$USER
  -help              Show this help
  -debug             Enable shell debug output
USAGE

  [ "$#" -gt 0 ] && exit "$1"
}

[ $# -eq 0 ] && usage 1

while [ $# -gt 0 ]; do
  case "$1" in
    '-env')
      shift
      ENV=$1
      shift
      ;;
    '-test-package')
      shift
      GO_TEST_PACKAGE_ARGS=$1
      shift
      ;;
    '-debug')
      shift
      DEBUG=true
      ;;
    '-cloud')
      shift
      CLOUD=$1
      shift
      ;;
    '-region')
      shift
      REGION=$1
      shift
      ;;
    '-expected-k8s')
      shift
      EXPECTED_K8S=$1
      shift
      ;;
    '-commit')
      shift
      COMMIT=$1
      shift
      ;;
    '-pr-id')
      shift
      PR_ID=$1
      shift
      ;;
    '-ssh-key')
      shift
      SSH_KEY=$1
      shift
      ;;
    '-ssh-user')
      shift
      SSH_USER=$1
      shift
      ;;
    '-help'|'-h'|'-?')
      usage 0
      ;;
    *)
      echo "Unknown Option $1"
      usage 1
      ;;
  esac
done

[ -z "${ENV}" ] && usage 1

export DATADOG_BASE_URL='https://ccloud-local-dev.datadoghq.com/'
export DATADOG_KAFKA_DETAIL_ID='epa-i4i-k8i'
export KIBANA_BASE_URL='https://devel.logs.aws.confluent.cloud/'
export KIBANA_INDEX='73f14690-4679-11e9-a586-991d8fddfb2e'
if [[ "${ENV}" = "devel" ]]; then
  export DATADOG_BASE_URL='https://ccloud-development.datadoghq.com/'
  export DATADOG_KAFKA_DETAIL_ID='8m3-z7n-idu'
  export KIBANA_BASE_URL='https://devel.logs.aws.confluent.cloud/'
  export KIBANA_INDEX='a4e307c0-4679-11e9-ac38-85968bb5acdc'
elif [[ "${ENV}" = "stag" ]]; then
  export DATADOG_BASE_URL='https://ccloud-staging.datadoghq.com/'
  export DATADOG_KAFKA_DETAIL_ID='wem-4dr-fyf'
  export KIBANA_BASE_URL='https://stag.logs.aws.confluent.cloud/'
  export KIBANA_INDEX='73f14690-4679-11e9-a586-991d8fddfb2e'
elif [[ "${ENV}" = "prod" ]]; then
  export DATADOG_KAFKA_DETAIL_ID='nre-iwx-b4h'
  export DATADOG_BASE_URL='https://ccloud-production.datadoghq.com/'
  export KIBANA_BASE_URL='https://prod.logs.aws.confluent.cloud/'
  export KIBANA_INDEX='7ae0cc50-dcdc-11ea-b484-556ef92a2241'
fi

_get_ssh_agent_env() {
  socket=$1;

  # check if there's already an ssh-agent running
  if pid=$(pgrep -f "ssh-agent -a ${socket}"); then
    SSH_AUTH_SOCK=${socket}; export SSH_AUTH_SOCK;
    SSH_AGENT_PID=${pid}; export SSH_AGENT_PID;
    echo "Agent pid ${pid}";
  else
    test -e "${socket}" && rm -f "${socket}"
    eval "$(ssh-agent -a "${socket}")"
  fi
}

_load_ssh_keys() {
  user_key=$1; shift
  [ $# -gt 0 ] && github_key=$1; shift

  # load user ssh key from file
  ssh-add -l | grep -q "${user_key}" || ssh-add -k "${user_key}"

  # load github key if it's set and exists
  if [ -n "${github_key}" ] && [ -f "${github_key}" ]; then
    ssh-add -l | grep -q "${github_key}" || ssh-add -k "${github_key}"
  fi
}

_export_connect_aws_access_key_id() {
  CONNECT_AWS_ACCESS_KEY_ID=$(aws configure --profile caas-"${ENV}" get aws_access_key_id)
  export CONNECT_AWS_ACCESS_KEY_ID
}

_export_aws_secret_access_key() {
  CONNECT_AWS_SECRET_ACCESS_KEY=$(aws configure --profile caas-"${ENV}" get aws_secret_access_key)
  export CONNECT_AWS_SECRET_ACCESS_KEY
}

[ "$DEBUG" = true ] && set -x
set -e

echo "Configuring SSH Keys"
_get_ssh_agent_env '/tmp/cc-system-tests.socket'
_load_ssh_keys "${SSH_KEY}" "${GITHUB_KEY}"
echo

echo "Configuring AWS credentials for Connect"
_export_connect_aws_access_key_id
_export_aws_secret_access_key

COMMIT=${COMMIT:-master}

echo "################################################################################"
echo "# Testing ${ENV}"
echo "################################################################################"

if [ "$ENV" = "prod" ]; then
  echo "SYSTEM TESTS CANNOT BE RUN IN PRODUCTION"
  #domain='confluent.cloud'
  exit 1
else
  domain="${ENV}.cpdev.cloud"
fi

[ "$DEBUG" = true ] && REMOTE_DEBUG="set -x"

if [ "$GO_TEST_PACKAGE_ARGS" = "./test/paas/..." ]; then
  echo "Running PAAS system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/orch/..." ]; then
  echo "Running ORCH system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/public_api/..." ]; then
  echo "Running PublicAPI system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/connect/..." ]; then
  echo "Running CONNECT system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/ksql/..." ]; then
  echo "Running KSQL system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/kafka/..." ]; then
  echo "Running Kafka system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/sr/..." ]; then
  echo "Running Schema Registry system tests..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/cli/..." ]; then
  echo "Running CLI system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/auditlog/..." ]; then
  echo "Running AuditLog system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/qe/..." ]; then
  echo "Running QE system test..."
elif [ "$GO_TEST_PACKAGE_ARGS" = "./test/ciam/..." ]; then
  echo "Running CIAM system test..."
elif [ -z "${GO_TEST_PACKAGE_ARGS}" ]; then
  echo "Running main system tests..."
  echo "Skipping PAAS system test in the run..."
  echo "Skipping KSQL system test in the run..."
  echo "Skipping Kafka system test in the run..."
  echo "Skipping Schema Registry system tests in the run..."
  echo "Skipping CLI system test in the run..."
  echo "Skipping AuditLog system test in the run..."
  echo "Skipping QE system test in the run..."
  echo "Skipping CIAM system test in the run..."
  echo "Skipping PublicAPI system test in the run..."
  # this is a temporary solution to disable sub package tests in main run
  # note that ./test/connect skipping is controlled by the CONNECT_SKIP_TESTS env var, also removing it here for consistency
  GO_TEST_PACKAGE_ARGS="./test $(find ./test -mindepth 1 -maxdepth 1 -type d \( ! -name paas \) \( ! -name orch \) \( ! -name ksql \) \( ! -name kafka \) \( ! -name sr \) \( ! -name cli \) \( ! -name auditlog \) \( ! -name connect \) -exec echo {}/... \; | xargs)"
fi

expected_k8s="${EXPECTED_K8S:-k8s-sz-a1}.${REGION}.${CLOUD}.internal.${domain}"
mothership_k8s="k8saas-eks-mothership-${ENV}"

# env used by test_helpers.go
export CCLOUD_ENV=${ENV}
export CCLOUD_URL=https://${domain}
export CREATE_CLOUD=${CLOUD}
export CREATE_REGION=${REGION}
export CREATE_EXPECTED_K8S=${expected_k8s}
export MOTHERSHIP_K8S=${mothership_k8s}
export CLOUD
AWS_REGION="${REGION}"
export AWS_REGION

# set semaphore ci user in based on env
if [ "${CI}" = true ]; then
  # export arn to accordingly AWS_STS_ASSUME_ROLE
  . vault-sem-get-secret AWSCLOUDROLEARN_"${ENV}"
  eval "$(aws sts assume-role --role-arn "${AWS_STS_ASSUME_ROLE}" --role-session-name cc-system-tests-"${ENV}" --duration-seconds 28800 | jq -r '.Credentials | "export AWS_ACCESS_KEY_ID=\(.AccessKeyId)\nexport AWS_SECRET_ACCESS_KEY=\(.SecretAccessKey)\nexport AWS_SESSION_TOKEN=\(.SessionToken)\n"')"

  ${SCRIPT_PATH}/setup_kubeconfig.sh "${ENV}"

  echo "Testing the mothership context: ${MOTHERSHIP_K8S}"
  if ! kubectl --context "${MOTHERSHIP_K8S}" get ns; then
    echo "Failed getting the ${MOTHERSHIP_K8S} namespaces"
    exit 1
  fi
fi

echo set TESTS_TO_RUN="${TESTS_TO_RUN}"
if [ -n "${CCLOUD_PRO_USER_EMAIL_PREFIX}" ]; then
  export CCLOUD_PRO_USER_EMAIL_PREFIX=${CCLOUD_PRO_USER_EMAIL_PREFIX}
fi
if [ -n "${CCLOUD_ENT_USER_EMAIL_PREFIX}" ]; then
  export CCLOUD_ENT_USER_EMAIL_PREFIX=${CCLOUD_ENT_USER_EMAIL_PREFIX}
fi
${REMOTE_DEBUG}

echo set GO_TEST_ARGS="${GO_TEST_ARGS}"
echo Running tests
echo set GO_TEST_PACKAGE_ARGS="${GO_TEST_PACKAGE_ARGS}"
if [ "${SKIP_INIT_ENV_STEP}" != "true" ]; then
  echo VERBOSE=${INIT_ENV_STEP_VERBOSE} make init-env
  VERBOSE=${INIT_ENV_STEP_VERBOSE} make init-env
fi
echo VERBOSE=${TEST_STEP_VERBOSE} make test
VERBOSE=${TEST_STEP_VERBOSE} make test
exit_code=$?

echo "Service logs at https://${ENV}.logs.aws.confluent.cloud/"
echo "Datadog metrics at ${DATADOG_BASE_URL}"
echo "Kibana logs at ${KIBANA_BASE_URL}"

exit ${exit_code}
