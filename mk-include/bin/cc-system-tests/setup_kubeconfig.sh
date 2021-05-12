#!/bin/bash

SCRIPT_NAME="$0"
SCRIPT_PATH="$(cd "$(dirname "$0")"; pwd)"

set -e -o pipefail -u

ENV="$1"

CLOUD=${CLOUD:-aws}
REGION=${REGION:-us-west-2}
AWS_REGION="${REGION}"
export AWS_REGION

# install aws-iam-authenticator
BIN_PATH="${HOME}/bin"
if [ ! -x "${BIN_PATH}/aws-iam-authenticator" ]; then
  curl -sLo aws-iam-authenticator https://amazon-eks.s3.us-west-2.amazonaws.com/1.18.8/2020-09-18/bin/linux/amd64/aws-iam-authenticator
  chmod +x ./aws-iam-authenticator
  mkdir -p "${HOME}"/bin
  mv ./aws-iam-authenticator "${BIN_PATH}/aws-iam-authenticator"
fi
export PATH="${PATH}:${BIN_PATH}"

# set semaphore ci user in based on env
if [ "${CI}" = true ]; then
  # export arn to accordingly AWS_STS_ASSUME_ROLE
  . vault-sem-get-secret AWSCLOUDROLEARN_"${ENV}"
  expected_role="$(echo ${AWS_STS_ASSUME_ROLE} | cut -f 2 -d /)"
  current_role="$(aws sts get-caller-identity | jq -r .Arn | cut -f 2 -d /)"
  if [ "${expected_role}" == "${current_role}" ]; then
    echo "Current AWS role is OK: ${expected_role}=${current_role}" 1>&2
  else
    echo "Assuming AWS role: ${current_role}=>${expected_role}" 1>&2
    eval "$(aws sts assume-role --role-arn "${AWS_STS_ASSUME_ROLE}" --role-session-name cc-system-tests-"${ENV}" --duration-seconds 28800 | jq -r '.Credentials | "export AWS_ACCESS_KEY_ID=\(.AccessKeyId)\nexport AWS_SECRET_ACCESS_KEY=\(.SecretAccessKey)\nexport AWS_SESSION_TOKEN=\(.SessionToken)\n"')"
  fi
fi

# initialize kubeconfig
if [ ! -f ~/.kube/config ]; then
  mkdir -p ~/.kube
  KOPS_STATE_STORE=s3://cloud.cpdev."${ENV}".kops
  export KOPS_STATE_STORE
  source ${SCRIPT_PATH}/kube_setup.sh; FORCE_KUBECONFIG_UPDATE=1; configure_kubecfg
fi
