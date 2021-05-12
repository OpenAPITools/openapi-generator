#!/bin/bash
# shellcheck shell=bash

assume_role() {
  local arn=$1
  local session_name=$2

  local response rc
  response="$(
    aws sts assume-role \
      --role-arn "${arn}" \
      --role-session-name "${session_name}" \
      --output text \
      --query '[Credentials.AccessKeyId,Credentials.SecretAccessKey,Credentials.SessionToken,Credentials.Expiration]')"
  rc=$?

  AWS_ACCESS_KEY_ID="$(echo "${response}" | awk '{print $1}')"
  AWS_SECRET_ACCESS_KEY="$(echo "${response}" | awk '{print $2}')"
  AWS_SESSION_TOKEN="$(echo "${response}" | awk '{print $3}')"
  export AWS_ACCESS_KEY_ID
  export AWS_SECRET_ACCESS_KEY
  export AWS_SESSION_TOKEN
  export AWS_DEFAULT_REGION=us-west-2
  export AWS_REGION=us-west-2
  return $rc
}
