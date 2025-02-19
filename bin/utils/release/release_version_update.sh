#!/usr/bin/env bash
#
# This script is used to update files to the "latest" version.
#
# usage: ./bin/utils/release_version_update.sh <from> <to>
# example: ./bin/utils/release_version_update.sh 3.0.1-SNAPSHOT 3.0.1
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
USAGE: $0 target

  This script will convert the current version in target files to the specified 'target'
  where target is one of:

  major
  minor
  build
  snapshot

EXAMPLES:

Update to new snapshot (1.0.0 -> 1.0.1-SNAPSHOT):
    $0 snapshot
Update build version (1.0.0 -> 1.0.1)
    $0 build
Update minor version (1.2.3 -> 1.3.0)
    $0 minor
Update major version (1.2.3 -> 2.0.0)
    $0 major
"

version=$(ruby -r rexml/document -e 'include REXML;
     p XPath.first(Document.new($stdin), "/project/version/text()")' < ${cwd}/../../../pom.xml | tr -d '"')

if [[ -n "$1" ]]; then
    case $1 in
        --help|-h)
            echo -e "$USAGE" >&2
            exit 1
            ;;
        major|minor|build|snapshot)
            inc="$1"
            ;;
        *)
            echo "Invalid target.Must be one of: major minor build or snapshot" >&2
            exit 1
            ;;
    esac
else
    inc="snapshot"
fi

echo "Release preparation: Moving from $version to next $inc version."

# These files should wrap target version replacement blocks with <!-- RELEASE_VERSION --> and <!-- /RELEASE_VERSION -->
# We can include xml and md files here.
declare -a xml_files=(
    "${root}/modules/openapi-generator-cli/pom.xml"
    "${root}/modules/openapi-generator-gradle-plugin/pom.xml"
    "${root}/modules/openapi-generator-core/pom.xml"
    "${root}/modules/openapi-generator-maven-plugin/pom.xml"
    "${root}/modules/openapi-generator-online/pom.xml"
    "${root}/modules/openapi-generator/pom.xml"
    "${root}/modules/openapi-generator-gradle-plugin/gradle.properties"
    "${root}/modules/openapi-generator-gradle-plugin/samples/local-spec/gradle.properties"
    "${root}/modules/openapi-generator-maven-plugin/examples/multi-module/java-client/pom.xml"
    "${root}/modules/openapi-generator-maven-plugin/examples/java-client.xml"
    "${root}/modules/openapi-generator-maven-plugin/examples/non-java-invalid-spec.xml"
    "${root}/modules/openapi-generator-maven-plugin/examples/non-java.xml"
    "${root}/modules/openapi-generator-maven-plugin/examples/kotlin.xml"
    "${root}/modules/openapi-generator-maven-plugin/examples/spring.xml"
    "${root}/pom.xml"
)

# These files should wrap target version replacement blocks with # RELEASE_VERSION and # /RELEASE_VERSION
declare -a properties_files=(
    "${root}/modules/openapi-generator-gradle-plugin/gradle.properties"
    "${root}/modules/openapi-generator-gradle-plugin/samples/local-spec/gradle.properties"
)

${cwd}/bump.sh -f ${version} -i ${inc} ${xml_files[@]}
${cwd}/bump.sh -f ${version} -t ${inc} -s '# RELEASE_VERSION' -e '# \/RELEASE_VERSION' ${properties_files[@]}
