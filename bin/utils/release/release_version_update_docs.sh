#!/usr/bin/env bash
#
# This script is used to update reference files to the "next" version.
#
# usage: ./bin/utils/release_version_update.sh <from> <to>
# example: ./bin/utils/release_version_update.sh 3.0.1 3.0.2
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

declare cwd=$(cd $(dirname "${BASH_SOURCE}") && pwd)
declare root=$(cd "$cwd" && cd ../../../ && pwd)

USAGE="
USAGE: $0 version target

  This script will convert the specified version in DOC target files to the 'target'
  where target is one of:

      major
      minor
      build

  or an explicitly defined version number.

NOTE:

  Files prepped by this script should never target SNAPSHOT, as the docs should refer to release artifacts.
  If intending to update to/from snapshots, please add target files to release_version_update.sh instead.

EXAMPLES:

Update build version (1.0.0 -> 1.0.1)
    $0 1.0.0 build
Update minor version (1.2.3 -> 1.3.0)
    $0 1.2.3 minor
Update major version (1.2.3 -> 2.0.0)
    $0 1.2.3 major
"

declare version=$(ruby -r rexml/document -e 'include REXML;
     p XPath.first(Document.new($stdin), "/project/version/text()")' < ${cwd}/../../../pom.xml  | tr -d '"')

declare target="${2:-build}"
declare inc=""
declare next_version=""
declare ags=""

if [[ -z "$1" ]]; then
    echo "Missing argument." >&2
    echo -e "$USAGE" >&2
    exit 1
fi

# Get version number we're changing
case $1 in
    --help|-h)
        echo -e "$USAGE" >&2
        exit 1
        ;;
    *)
        version="$1"
        ;;
esac

# Get the target…
case ${target} in
    major|minor|build)
        inc="$target"
        ;;
    snapshot)
        echo -e "Files prepped by this script should never target SNAPSHOT, as the docs should refer to release artifacts.
        If intending to update to/from snapshots, please add target files to release_version_update.sh instead.
        " >&2
        exit 1
        ;;
    *)
        next_version="$target"
        ;;
esac

ags="-f ${version}"

if [[ -n "${next_version}" ]];then
    echo "Release preparation: Moving from $version to ${next_version}."
    ags="$ags -t ${next_version}"
else
    echo "Release preparation: Moving from $version to next $inc version."
    ags="$ags -i ${inc}"
fi

declare -a xml_files=(
    "${root}/modules/openapi-generator-maven-plugin/README.md"
    "${root}/modules/openapi-generator-gradle-plugin/README.adoc"
    "${root}/modules/openapi-generator-gradle-plugin/samples/local-spec/README.md"
    "${root}/README.md"
    "${root}/docs/installation.md"
    "${root}/website/src/pages/index.js"
)

declare -a commented_files=(
    "${root}/modules/openapi-generator-gradle-plugin/README.adoc"
)

${cwd}/bump.sh ${ags} ${xml_files[@]}
${cwd}/bump.sh ${ags} -s '# RELEASE_VERSION' -e '# \/RELEASE_VERSION' ${commented_files[@]}
