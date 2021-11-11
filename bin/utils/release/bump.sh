#!/usr/bin/env bash
#
# This script bumps from one version to another
#
# Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

declare -r version_regex="([0-9]+).([0-9]+).([0-9]+)-?(SNAPSHOT){0,1}"
declare start="<!-- RELEASE_VERSION -->"
declare end="<!-- \/RELEASE_VERSION -->"
declare from="${version_regex}"
declare to=""
declare debug=${debug:-false}
declare -a from_parts=()
declare -a to_parts=()
declare -ar inc=(major minor build snapshot)

USAGE="
USAGE: $0 OPTIONS input_file

  This script will bump a version number (or other value) between marker tags.

OPTIONS:
    -f The 'from' version
    -t The 'to' version
    -s The start tag regex
       default: $start
    -e The end tag regex
       default: $end
    -i Increase by one of: ${inc[@]}
    -h Print this message

EXAMPLES:

Update to next snapshot version:
    $0 -f 3.0.0 -t 3.0.1-SNAPSHOT pom.xml
Update build version only (useful for docs)
    $0 -f 3.0.0 -t 3.0.1 pom.xml
Update from any version to any other version
    $0 -f 1.2.3 -t 9.9.9-SNAPSHOT pom.xml
Customize the start/end tags
    $0 -f 1.0.0 1.0.1-SNAPSHOT -s \"<!-- START -->\" -e \"<!-- END -->\" pom.xml
"


## print an error message and exit
err() {
    >&2 echo -e "$1"
    exit 1
}

## debug log messages. Run with debug=true ./bump.sh
d() {
    if [[ true = "${debug}" ]]; then
        echo "$1"
    fi
}

## outputs usage and exits
usage()
{
    err "${USAGE}"
    exit 1
}

## usage: version input extracted_array
##   - Checks that 'input' is a valid version
##   - Extracts the version parts into 'extracted_array'
version()
{
    if [[ "$#" -ne 2 ]]; then
        err "Call function version with two parameters: version string arr"
    fi
    local v=$1
    if [[ "${v}" =~ $version_regex ]]; then
        local major=${BASH_REMATCH[1]}
        local minor=${BASH_REMATCH[2]}
        local build=${BASH_REMATCH[3]}
        local snapshot=false
        if [[ "SNAPSHOT" = "${BASH_REMATCH[4]}" ]]; then
            snapshot=true
        fi

        d "major=$major minor=$minor build=$build snapshot=$snapshot"

        eval "$2=(${major} ${minor} ${build} ${snapshot})"
    else
        err "Invalid version: $v"
    fi
}

while getopts "hf:t:s:e:i:" OPTION
do
  case ${OPTION} in
    f)
        from=${OPTARG}
        ;;
    t)
        to=${OPTARG}
        ;;
    s)
        start=${OPTARG}
        ;;
    e)
        end=${OPTARG}
        ;;
    i)
        increase=${OPTARG}
        if [[ ! "${inc[@]}" =~ ${increase} ]];then
            err "Only support increasing by one of: ${inc[@]}"
        fi
        ;;
    h)
        usage
        ;;
  esac
done

shift $((OPTIND-1))
file=( "$@" )

if [[ ${#file[@]} -eq 0 ]];then
   echo "No file specified" >&2
   usage
fi

if [[ -z "${from}" ]]; then
   echo  "No 'from' version specified." >&2
   usage
fi

# TODO: compare steps in from_parts and to_parts.
version "${from}" from_parts

if [[ -z "${to}" ]]; then
   if [[ -z "${increase}" ]]; then
    err "No 'to' version specified."
   else
    case ${increase} in
        major)
            to="$(( ${from_parts[0]} + 1 )).0.0"
            version "$to" to_parts
            ;;
        minor)
            to="${from_parts[0]}.$(( ${from_parts[1]} + 1 )).0"
            version "$to" to_parts
            ;;
        build)
            to="${from_parts[0]}.${from_parts[1]}.$(( ${from_parts[2]} + 1 ))"
            version "$to" to_parts
            ;;
        snapshot)
            if [[ true = ${from_parts[3]} ]]; then
                # Going from -SNAPSHOT to its release
                to="${from_parts[0]}.${from_parts[1]}.${from_parts[2]}"
            else
                # Going from some version to its next version and -SNAPSHOT
                to="${from_parts[0]}.${from_parts[1]}.$(( ${from_parts[2]} + 1 ))-SNAPSHOT"
            fi
            version "$to" to_parts
            ;;
    esac
   fi
else
    version "${to}" to_parts
fi

if [[ ${from_parts[3]} = true && ${to_parts[3]} = true ]]; then
    err "Moving from SNAPSHOT to SNAPSHOT is not supported."
fi

cat <<EOF > sedscript.sed
/${start}/,/${end}/{
    s/${from}/${to}/g
}
EOF

d "Moving from=${from} to=${to}"

trap 'rm -f sedscript.sed' EXIT

sed_cross() {
  # Cross-platform sed invocation. OSX has no option to show a version number in sed.
  local target=$1
  sed --version >/dev/null 2>&1 && sed -e -i '' -f sedscript.sed "$target" || sed -i '' -E -f sedscript.sed "$target"
}

update_file() {
    local filename=$1
    local error_message="ERROR: Failed to update $filename to target version ${to}"
    local original_hash=$(ruby -r digest -e "p Digest::SHA2.file(\"$filename\").hexdigest")
    local final_hash=""
    if ! sed_cross ${filename}; then
        # occurs if, for example, the file doesn't exist.
        echo "ERROR: Failed to update $filename to target version ${to}" >&2
    fi

    local final_hash=$(ruby -r digest -e "p Digest::SHA2.file(\"$filename\").hexdigest")

    if [[ "${original_hash}" = "${final_hash}" ]]; then
        # occurs if, for example, the file doesn't have expected marker tags for replacement
        echo "ERROR: $filename was not modified." >&2
    else
        echo "Updated $filename successfully!"
    fi
}

for filename in "${file[@]}"; do
    update_file ${filename}
done

