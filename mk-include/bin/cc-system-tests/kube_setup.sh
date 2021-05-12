#!/bin/bash

# Determine shell
if [ ! -z "$BASH_VERSION" ]; then
  _shell=bash
elif [ ! -z "$ZSH_VERSION" ]; then
  _shell=zsh
fi

__configure_kubecfg_root_tmpdir="${TMPDIR:-/tmp}/configure_kubecfg.tmp.${USER}"

# Creates a temporary config dir only once.
__configure_kubecfg_mktemp() {
  __configure_kubecfg_tmpdir="${__configure_kubecfg_root_tmpdir}/$$"
  mkdir -p "${__configure_kubecfg_tmpdir}"
  echo "${__configure_kubecfg_tmpdir}"
}

__configure_kubecfg_rmtemp() {
  __configure_kubecfg_tmpdir="${__configure_kubecfg_root_tmpdir}/$$"
  rm -rf "${__configure_kubecfg_tmpdir:-/tmp/nop}"
}

__configure_kubecfg_purgetemp() {
  # Delete any dir older than 1 day
  [ -n "${VERBOSE:-}" ] && echo "Purging files in ${__configure_kubecfg_root_tmpdir:-/tmp/nop}" 1>&2
  find "${__configure_kubecfg_root_tmpdir:-/tmp/nop}" -maxdepth 1 -mindepth 1 -type d -mtime 1 -delete

  # Try delete root dir, will delete if empty
  rmdir "${__configure_kubecfg_root_tmpdir}" > /dev/null 2>&1 || true
}


# Merges several kubecfg config files, including the target
# $1: target kubecfg
# $@: rest of files
__configure_kubecfg_merge() {
  local target_kubeconfig="$1"; shift
  local config_files="$@"

  [ -n "${VERBOSE:-}" ] && echo "Merging kubecfg config into ${target_kubeconfig}" 1>&2

  local tmpdir
  tmpdir="$(__configure_kubecfg_mktemp)"
  local tmp_kubeconfig="${tmpdir}/kubeconfig.merging"

  # Merge all the files AND the target file into a temporary one
  KUBECONFIG="$(echo ${config_files} | xargs | tr ' ' :):${target_kubeconfig}" \
    kubectl config view --raw > "${tmp_kubeconfig}"

  mv "${tmp_kubeconfig}" "${target_kubeconfig}"
}


# Configures in parallel multiple kubectl clusters, merging the config
# at the end. It receives two functions to list and configure one cluster
#
# $1: target kubecfg file
# $2: function name to configure one cluster. Will receive:
#    - target_kubeconfig_dir where the config must be writing in a unique file
#    - one line of the list function
# $3: function name to list clusters.
# $4: argument for the list function (e.g. region)
__configure_kubecfg_parallel_run() {
  local target_kubeconfig="$1"
  local configure_single_cluster_func="$2"
  local list_clusters_func="$3"
  local list_clusters_arg="$4"

  # How many clusters configure in parallel
  local CONFIGURE_KUBECFG_PARALLEL="${CONFIGURE_KUBECFG_PARALLEL:-10}"

  # setup temporary directory for the temporary files and delete it on exit
  local tmpdir
  tmpdir="$(__configure_kubecfg_mktemp)"
  local tmp_kubeconfig_dir="${tmpdir}/clusters"
  mkdir -p "${tmp_kubeconfig_dir}"

  # Lists the cluster using ${list_clusters_func} and
  # call ${configure_single_cluster_func} to configure each cluster,
  # using xargs parallel execution feature.
  #
  # We wrap the call in a ${_shell} subshell because xargs does not
  # work with functions.
  #
  "${list_clusters_func}" "${list_clusters_arg}" | \
    xargs -I{} -P"${CONFIGURE_KUBECFG_PARALLEL}" \
      "${_shell}" -c "
        $(declare -f "${configure_single_cluster_func}");
        '${configure_single_cluster_func}' '${tmp_kubeconfig_dir}' '{}';
      "

  # Merge all the resulting files
  __configure_kubecfg_merge "${target_kubeconfig}" "${tmp_kubeconfig_dir}"/*
}

__configure_kubecfg_aws_kops_single_cluster() {
  set -e -u -o pipefail;
  [ -n "${DEBUG:-}" ] && set -x;
  local target_kubeconfig_dir="$1"
  local kops_cluster_name="$2"

  [ -n "${VERBOSE:-}" ] && echo "Configuring kops kubectl ${kops_cluster_name}" 1>&2
  if ! KUBECONFIG="${target_kubeconfig_dir}/${kops_cluster_name}" \
    kops export kubecfg "${kops_cluster_name}" > /dev/null ; then
    echo "ERROR: Failed retrieving config for ${kops_cluster_name}" 1>&2
    return 1
  fi
}

__configure_kubecfg_aws_kops_list_clusters() {
  local aws_region="$1"

  # Try the fastest approach of listing all the clusters from
  # the bucket ${KOPS_STATE_STORE} that have <name>/config
  if [ -z "${USE_KOPS_LIST_CLUSTERS:-}" ]; then
      s3_found_kops_clusters="$(
        aws s3api list-objects \
          --bucket "${KOPS_STATE_STORE##s3://}" | \
              jq --arg region ${aws_region} -r \
              '
                [ .Contents[] | .Key |
                  select(
                    contains("."+$region+".")
                    and
                    endswith("/config")
                  ) | split("/")[0]
                ] | unique | .[]' || true
      )"
  fi

  if [ -z "${USE_KOPS_LIST_CLUSTERS:-}" -a -n "${s3_found_kops_clusters:-}" ]; then
    echo  "${s3_found_kops_clusters}"
  else
    # Fallback to slower get clusters with kops
    echo "WARNING: Unable to list kops clusters from ${KOPS_STATE_STORE}. Failing over to plain 'kops'" 1>&2
    kops get clusters -o json | jq -r '.[].metadata.name' | grep ".${aws_region}."
  fi
}

__configure_kubecfg_aws_kops() {
  [ -n "${DEBUG:-}" ] && set -x

  local target_kubeconfig="$1"
  local aws_region="$2"

  echo "Configuring kubectl for AWS KOPS in ${aws_region}..." 1>&2

  __configure_kubecfg_parallel_run \
      "${target_kubeconfig}" \
      __configure_kubecfg_aws_kops_single_cluster \
      __configure_kubecfg_aws_kops_list_clusters \
      "${aws_region}"
}

__configure_kubecfg_gcp_gke_single_cluster() {
  set -e -u -o pipefail;
  [ -n "${DEBUG:-}" ] && set -x;
  local target_kubeconfig_dir="$1"
  local gke_cluster="$2"

  local project=$(echo "${gke_cluster}" | cut -f 1 -d /)
  local zone=$(echo "${gke_cluster}" | cut -f 3 -d /)
  local gke=$(echo "${gke_cluster}" | cut -f 5 -d /)

  local target_kubeconfig="${target_kubeconfig_dir}/${project}_${zone}_${gke}"

  [ -n "${VERBOSE:-}" ] && echo "Configuring GCP GKE kubectl for ${gke_cluster}" 1>&2
  if ! KUBECONFIG="${target_kubeconfig}" \
    gcloud --quiet container clusters get-credentials \
      --project "${project}" --zone "${zone}" "${gke}" >/dev/null 2>&1;
  then
    # Rerun to print the error
    echo "Failure configuring ${gke_cluster}" 1>&2
    echo "Output of: gcloud --quiet container clusters get-credentials --project ${project} --zone ${zone} ${gke}" 1>&2
    KUBECONFIG="${target_kubeconfig}" \
    gcloud --quiet container clusters get-credentials \
      --project "${project}" --zone "${zone}" "${gke}" 2>&1 || true
    echo "Skipping ${gke_cluster}" 1>&2
    return 0
  fi

  if [[ "$gke" == c-* ]]; then
      local display_name="$(
        gcloud container clusters describe \
            "${gke}" --project "${project}" \
            --zone "${zone}" \
            --format='value(resourceLabels.display-name)')"
      local base_name="$(echo ${display_name//--/ } | cut -f 1 -d ' ')"
      local region="$(echo "${zone}" | sed 's/-.$//')"
      local gke_old_name="gke_${project}_${zone}_${gke}"
      local gke_new_name="gke_${project}_${zone}_${base_name}-${region}"
      [ -n "${VERBOSE:-}" ] && echo "Renaming kubectl context ${gke_old_name} => ${gke_new_name}" 2>&2
      KUBECONFIG="${target_kubeconfig}" \
      kubectl config rename-context \
          "${gke_old_name}" \
          "${gke_new_name}" >/dev/null
  fi;
}

__configure_kubecfg_gcp_gke_list_clusters() {
  local gcp_region="$1"

  [ -z "${GCP_PROJECTS:-}" ] && \
    GCP_PROJECTS="$(gcloud projects list --format 'value(project_id)')"

  [ -n "${VERBOSE:-}" ] && echo "Listing clusters from GCE projects ${GCP_PROJECTS/$'\n'/ }" 1>&2
  for gcp_project in ${GCP_PROJECTS}; do
    gcloud container clusters list \
      --project "${gcp_project}" \
      --filter="zone ~ ^$gcp_region" \
      --format='value(selfLink.scope(projects))'
  done
}

__configure_kubecfg_gcp_gke() {
  [ -n "${DEBUG:-}" ] && set -x

  local target_kubeconfig="$1"
  local gcp_region="$2"

  echo "Configuring kubectl for GCP GKE in ${gcp_region}..." 1>&2
  __configure_kubecfg_parallel_run \
      "${target_kubeconfig}" \
      __configure_kubecfg_gcp_gke_single_cluster \
      __configure_kubecfg_gcp_gke_list_clusters \
      "${gcp_region}"
}

__configure_kubecfg_azure_aks_single_cluster() {
  set -e -u -o pipefail;
  [ -n "${DEBUG:-}" ] && set -x;
  local target_kubeconfig_dir="$1"
  local azure_cluster="$2"

  local rg="$(echo "${azure_cluster}" | cut -f 5 -d /)"
  local aks="$(echo "${azure_cluster}" | cut -f 9 -d /)"

  [ -n "${VERBOSE:-}" ] && echo "Configuring Azure AKS kubectl for ${aks}" 1>&2

  local context="$(
    az aks show \
      --name "${aks}" \
      --resource-group "${rg}" \
      -o json | \
        jq -r '.tags.Context'
  )"
  az aks get-credentials \
    -f "${target_kubeconfig_dir}/${aks}" \
    --admin --overwrite-existing \
    --resource-group "${rg}" \
    --name "${aks}" >/dev/null 2>&1

  # Change to standard naming conventions
  KUBECONFIG="${target_kubeconfig_dir}/${aks}" \
    kubectl config rename-context \
      "${aks}-admin" \
      "${context}" >/dev/null
}

__configure_kubecfg_azure_aks_list_clusters() {
  local azure_location="$1"

  az aks list  --query "[?location=='${azure_location}'].id" -o tsv
}

__configure_kubecfg_azure_aks() {
  [ -n "${DEBUG:-}" ] && set -x

  local target_kubeconfig="$1"
  local azure_location="$2"

  echo "Configuring kubectl for Azure AKS in ${azure_location}..." 1>&2
  az login --identity --out none
  __configure_kubecfg_parallel_run \
      "${target_kubeconfig}" \
      __configure_kubecfg_azure_aks_single_cluster \
      __configure_kubecfg_azure_aks_list_clusters \
      "${azure_location}"
}


__configure_kubecfg_rancher_cli_setup() {
  # Login to Rancher server
  local rancher_url="${RANCHER_URL:-$(sops -d --extract '["rancher"]["url"]' /etc/simple_secrets.sops.yaml || true)}"
  local rancher_token="${RANCHER_TOKEN:-$(sops -d --extract '["rancher"]["token"]' /etc/simple_secrets.sops.yaml || true)}"

  if [ -z "${rancher_url}" ] || [ -z "${rancher_token}" ]; then
    echo "ERROR: Failed retrieving rancher credentials. Pass RANCHER_URL and RANCHER_TOKEN or set it in /etc/simple_secrets.sops.yaml" 1>&2
    return 1
  fi

  mkdir -p ~/.rancher
  cat << EOF > ~/.rancher/cli2.json
{
      "Servers":
      {
          "rancherDefault":
          {
              "accessKey": "${rancher_token%%:*}",
              "secretKey":"${rancher_token#*:}",
              "tokenKey":"${rancher_token}",
              "url":"${rancher_url}",
              "project":"",
              "cacert":""
          }
      },
      "CurrentServer": "rancherDefault"
  }
EOF
}

__configure_kubecfg_rancher_list_clusters() {
  rancher cluster ls \
    --format '{{.Cluster.ID}}|{{.Cluster.Name}}|{{index .Cluster.Annotations "confluent.io/name"}}' 2> /dev/null
}

__configure_kubecfg_rancher_single_cluster() {
  [ -n "${DEBUG:-}" ] && set -x;
  set -e -u -o pipefail;
  local target_kubeconfig_dir="$1"
  local rancher_cluster_config="$2"

  [ -n "${VERBOSE:-}" ] && echo "Configuring Rancher kubectl for ${rancher_cluster_config}" 1>&2

  # Pair will be in '<id>|<name>' format, split it out
  local rancher_cluster_id=$(echo "${rancher_cluster_config}" | cut -d'|' -f1)
  local rancher_cluster_display_name=$(echo "${rancher_cluster_config}" | cut -d'|' -f2)
  local rancher_cluster_std_name=$(echo "${rancher_cluster_config}" | cut -d'|' -f3)

  local target_kubeconfig="${target_kubeconfig_dir}/${rancher_cluster_id}"

  # Export this clusters config
  rancher cluster kf "${rancher_cluster_id}" > "${target_kubeconfig}" 2> /dev/null

  # Change to standard naming conventions
  KUBECONFIG="${target_kubeconfig}" \
    kubectl config rename-context \
      "${rancher_cluster_display_name}" \
      "${rancher_cluster_std_name}" >/dev/null
}

__configure_kubecfg_rancher() {
  local target_kubeconfig="$1"

  # Setup rancher credentials
  __configure_kubecfg_rancher_cli_setup || return 1

  echo "Configuring kubectl for Rancher..." 1>&2

  # FIXME not sure about running this in parallel for rancher
  CONFIGURE_KUBECFG_PARALLEL=1

  __configure_kubecfg_parallel_run \
      "${target_kubeconfig}" \
      __configure_kubecfg_rancher_single_cluster \
      __configure_kubecfg_rancher_list_clusters \
      "nop"
}

__configure_kubecfg_aws_eks_single_cluster() {
  set -e -u -o pipefail;
  [ -n "${DEBUG:-}" ] && set -x;
  local target_kubeconfig_dir="$1"
  local eks_cluster="$2"

  local aws_region="$(echo "${eks_cluster}" | cut -f 1 -d :)"
  local eks="$(echo "${eks_cluster}" | cut -f 2 -d :)"
  local target_kubeconfig="${target_kubeconfig_dir}/${aws_region}_${eks}"

  local eks_status="$(
    aws eks describe-cluster \
      --region "${aws_region}" \
      --name "${eks}" \
      --query cluster.status \
      --output text
  )"
  if [ "${eks_status}" != "ACTIVE" ]; then
    echo "WARNING: Skipping '${eks}' in status '${eks_status}'" 1>&2
    return 0
  fi

  [ -n "${VERBOSE:-}" ] && echo "Configuring eks kubectl ${eks_cluster}" 1>&2
  if ! KUBECONFIG="${target_kubeconfig}" \
    aws eks update-kubeconfig \
      --region "${aws_region}" \
      --alias "${eks}" \
      --name "${eks}" > /dev/null; then

    echo "ERROR: Failed retrieving config for ${eks_cluster}" 1>&2
    return 1
  fi

  # Rename the context if it matches a name like:
  #
  #  k8s-rancher-sz-1--devel--ce49c21b88d03574
  #  k8s-foo-1--prod--abc1234567890
  #
  # To the expected format
  #
  #  k8s-rancher-sz-1.us-west-2.aws.internal.devel.cpdev.cloud
  #
  # See: https://confluentinc.atlassian.net/browse/CIRE-465
  #
  if expr "${eks//--/.}" : '^[a-z0-9-]\+\.[a-z]\+\.[a-z0-9-]\+$' > /dev/null; then
    local eks_basename="$(echo ${eks//--/ } | cut -f 1 -d ' ')"
    local eks_env="$(echo ${eks//--/ } | cut -f 2 -d ' ')"
    local eks_domain="$(
      echo "${eks_env}" | sed '
        s|devel|internal.devel.cpdev.cloud|;
        s|stag|internal.stag.cpdev.cloud|;
        s|prod|internal.confluent.cloud|;
        ')"
    local eks_new_name="${eks_basename}.${aws_region}.aws.${eks_domain}"

    [ -n "${VERBOSE:-}" ] && echo "Renaming kubectl context ${eks} => ${eks_new_name}" 1>&2
    KUBECONFIG="${target_kubeconfig}" \
      kubectl config rename-context \
          "${eks}" \
          "${eks_new_name}" >/dev/null
  fi
}

__configure_kubecfg_aws_eks_list_clusters() {
  local aws_region="$1"

  aws eks list-clusters --region "${aws_region}" | \
    jq --arg aws_region "${aws_region}" -r '$aws_region + ":" + .clusters[]'
}

__configure_kubecfg_aws_eks_is_supported() {
  aws eks list-clusters --region "$1" 2>&1 > /dev/null
}

__configure_kubecfg_aws_eks() {
  [ -n "${DEBUG:-}" ] && set -x

  local target_kubeconfig="$1"
  local aws_region="$2"

  if ! __configure_kubecfg_aws_eks_is_supported "${aws_region}"; then
    echo "WARNING: Unable to access EKS API. Region does not support EKS or lack of permissions. Skipping." 1>&2
    return 0
  fi

  echo "Configuring kubectl for AWS EKS in ${aws_region}..." 1>&2

  __configure_kubecfg_parallel_run \
      "${target_kubeconfig}" \
      __configure_kubecfg_aws_eks_single_cluster \
      __configure_kubecfg_aws_eks_list_clusters \
      "${aws_region}"
}


__configure_kubecfg_get_file_age_in_minutes() {
  local filename="$1"
  [ -f "${filename}" ] || return
  if [ "$(uname -s)" == "Darwin" ]; then
    echo "$(( ( "$(date +%s)" - "$(stat -t %s -f %m -- "$filename")" ) / 60 ))"
  else
    echo "$(( ( "$(date +%s)" - "$(date +%s -r "$filename")" ) / 60 ))"
  fi
}

configure_kubecfg() {
  (
  set -u -e -o pipefail
  [ -n "${DEBUG:-}" ] && set -x

  local target_kubeconfig="${KUBECONFIG:-${HOME}/.kube/config}"

  # Try to load a prepopulated kubeconfig file from shared location first
  if [ -z "${FORCE_KUBECONFIG_UPDATE:-}" ]; then
    local shared_kubecfg="${SHARED_KUBECONFIG:-/opt/kubeconfig}"
    local max_shared_kubeconfig_age=${MAX_SHARED_KUBECONFIG_AGE_IN_MIN:-60}
    local shared_kubecfg_age="$(__configure_kubecfg_get_file_age_in_minutes "${shared_kubecfg}")"

    if [ -f "${shared_kubecfg}" ] && [ "${shared_kubecfg_age}" -lt "${max_shared_kubeconfig_age}" ]; then
      echo "INFO: loading prepopulated file ${shared_kubecfg}, updated ${shared_kubecfg_age}m ago. Force generation by setting FORCE_KUBECONFIG_UPDATE=1" 1>&2
      mkdir -p "${target_kubeconfig%/*}"
      [ ! -f "${target_kubeconfig}" ] || cp "${target_kubeconfig}" "${target_kubeconfig}.bak"
      cp "${shared_kubecfg}" "${target_kubeconfig}"
      return
    else
      echo "WARNING: Unable to find a prepopulated ${shared_kubecfg} newer than ${max_shared_kubeconfig_age}m. Regenerating the file." 1>&2
    fi
  else
    echo "INFO: \$FORCE_KUBECONFIG_UPDATE was set. Regenerating file." 1>&2
  fi

  # Regenerate the kubeconfig file
  __configure_kubecfg_rmtemp
  trap '__configure_kubecfg_rmtemp; __configure_kubecfg_purgetemp' EXIT ERR
  local tmpdir
  tmpdir="$(__configure_kubecfg_mktemp)"
  local new_kubeconfig="${tmpdir}/kubeconfig"

  [ -n "${VERBOSE:-}" ] && echo "Using temporary dir ${tmpdir}" 1>&2

  # Get list of k8s clusters
  shopt -s nocasematch
  case "${CLOUD}" in
    'AWS')
      __configure_kubecfg_aws_kops "${new_kubeconfig}" "${AWS_REGION}" || true
      __configure_kubecfg_aws_eks "${new_kubeconfig}" "${AWS_REGION}" || true
      ;;
    'GCP')
      __configure_kubecfg_gcp_gke "${new_kubeconfig}" "${REGION}"
      ;;
    'AZURE')
      __configure_kubecfg_azure_aks "${new_kubeconfig}" "${REGION}"
      ;;
    *)
      echo "ERROR: Unknown Cloud ${CLOUD}"
      return 1
      ;;
  esac
  shopt -u nocasematch

  if [ -n "${ENABLE_RANCHER:-}" ]; then
    # Configure rancher in all clouds but swallow the failure
    __configure_kubecfg_rancher "${new_kubeconfig}" || ret="$?"
    if [ "${ret:-0}" != "0" ]; then
      echo "Failed loading rancher clusters, skipping."
    fi
  fi

  # Try to keep the previously selected context
  local previous_context="$(kubectl config current-context 2> /dev/null || true)"
  [ -n "${previous_context}" ] &&
    kubectl --kubeconfig "${new_kubeconfig}" \
      config use-context "${previous_context}" 2>&1 > /dev/null || true

  mkdir -p "${target_kubeconfig%/*}"
  [ ! -f "${target_kubeconfig}" ] || cp "${target_kubeconfig}" "${target_kubeconfig}.bak"
  cp "${new_kubeconfig}" "${target_kubeconfig}"

  )
  echo "Done: kubectl configured in ${KUBECONFIG:-${HOME}/.kube/config}" 1>&2
}

reload_kube_completion() {
  echo "Configuring kubectl completion"
  kubectl completion $_shell > ~/.${_shell}_kube_completion
  # shellcheck source=/dev/null
  source ~/.${_shell}_kube_completion
}

if [ ! -f ~/.kube/config ]; then
  configure_kubecfg
fi

if [ ! -f ~/.${_shell}_kube_completion ]; then
  reload_kube_completion
fi

__get_azure_aws_credentials() {
  (
  set -e -o pipefail -u
  vault_name="$(echo bastion-${CAAS_ENV}-${REGION} | tr A-Z a-z)"
  # Ensure we have a managed identity
  az login --identity > /dev/null

  # Get real vault searching by name or tag
  actual_vault_name="$(
    az keyvault list -o tsv \
      --query "[?(name=='${vault_name}' || tags.name=='${vault_name}')] | [0].name"
  )"

  awsaccesskeyid=$(az keyvault secret show --name "awsaccesskeyid" --vault-name "${actual_vault_name}" -o tsv --query 'value')
  awssecretaccesskey=$(az keyvault secret show --name "awssecretaccesskey" --vault-name "${actual_vault_name}" -o tsv --query 'value')
  if [ -z "${awsaccesskeyid}" -o -z "${awssecretaccesskey}" ]; then
    echo "Failed retrieving creds" 1>&2
    return 1
  fi

  echo "[default]"
  echo "aws_access_key_id = ${awsaccesskeyid}"
  echo "aws_secret_access_key = ${awssecretaccesskey}"
  )
}

# Azure is using s3 for terraform state as storage accounts do not have a versioning system
case "${CLOUD}" in
  'AZURE')
    if [ ! -f ~/.aws/credentials ]; then
      echo "Configuring S3 Credentials for Azure Bastion"
      mkdir -p ~/.aws
      __get_azure_aws_credentials > ~/.aws/credentials.tmp && mv ~/.aws/credentials.tmp ~/.aws/credentials
    fi
    ;;
  *)
    ;;
esac
