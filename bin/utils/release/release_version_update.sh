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
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

declare cwd=$(cd $(dirname "${BASH_SOURCE}") && pwd)

if [[ "$1" != "" ]]; then
    FROM="$1"
else
    echo "Missing argument. Usage e.g.: ./bin/utils/release_version_update.sh 3.0.1-SNAPSHOT 3.0.1"
    exit 1;
fi

if [[ "$2" != "" ]]; then
    TO="$2"
else
    echo "Missing argument. Usage e.g.: ./bin/utils/release_version_update.sh 3.0.1-SNAPSHOT 3.0.1"
    exit 1;
fi

echo "Release preparation: replacing $FROM with $TO in different files"

# These files should wrap target version replacement blocks with <!-- RELEASE_VERSION --> and <!-- /RELEASE_VERSION -->
# We can include xml and md files here.
declare -a xml_files=("modules/openapi-generator-cli/pom.xml"
                  "modules/openapi-generator-gradle-plugin/pom.xml"
                  "modules/openapi-generator-core/pom.xml"
                  "modules/openapi-generator-maven-plugin/pom.xml"
                  "modules/openapi-generator-online/pom.xml"
                  "modules/openapi-generator/pom.xml"
                  "samples/meta-codegen/lib/pom.xml"
                  "pom.xml")

# These files should wrap target version replacement blocks with # RELEASE_VERSION and # /RELEASE_VERSION
declare -a properties_files=(
    "modules/openapi-generator-gradle-plugin/gradle.properties"
)

for filename in "${xml_files[@]}"; do
  ${cwd}/bump.sh -f $FROM -t $TO $filename
done

for filename in "${properties_files[@]}"; do
  ${cwd}/bump.sh -f $FROM -t $TO -s '# RELEASE_VERSION' -e '# \/RELEASE_VERSION' $filename
done

