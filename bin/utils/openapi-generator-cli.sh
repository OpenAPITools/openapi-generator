#!/usr/bin/env bash
####
# Save as openapi-generator-cli on your PATH. chmod u+x. Enjoy.
#
# This script will query github on every invocation to pull the latest released version
# of openapi-generator.
#
# If you want repeatable executions, you can explicitly set a version via
#    OPENAPI_GENERATOR_VERSION
# e.g. (in Bash)
#    export OPENAPI_GENERATOR_VERSION=3.1.0
#    openapi-generator-cli.sh
# or
#    OPENAPI_GENERATOR_VERSION=3.1.0 openapi-generator-cli.sh
#
# This is also helpful, for example, if you want want to evaluate a SNAPSHOT version.
#
# NOTE: Jars are downloaded on demand from maven into the same directory as this script
# for every 'latest' version pulled from github. Consider putting this under its own directory.
####
set -o pipefail

for cmd in {mvn,python,curl}; do
  if ! command -v ${cmd} > /dev/null; then
  >&2 echo "This script requires '${cmd}' to be installed."
    exit 1
  fi
done

function latest.tag {
  local uri="https://api.github.com/repos/${1}/tags"
  curl -s ${uri} | python -c "import sys, json; print json.load(sys.stdin)[0]['name'][1:]"
}

ghrepo=openapitools/openapi-generator
groupid=org.openapitools
artifactid=openapi-generator-cli
ver=${OPENAPI_GENERATOR_VERSION:-$(latest.tag $ghrepo)}

jar=${artifactid}-${ver}.jar
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ ! -f ${DIR}/${jar} ]; then
  repo="central::default::https://repo1.maven.apache.org/maven2"
  if [[ ${ver} =~ ^.*-SNAPSHOT$ ]]; then
      repo="central::default::https://oss.sonatype.org/content/repositories/snapshots"
  fi
  mvn org.apache.maven.plugins:maven-dependency-plugin:2.9:get \
    -DremoteRepositories=${repo} \
    -Dartifact=${groupid}:${artifactid}:${ver} \
    -Dtransitive=false \
    -Ddest=${DIR}/${jar}
fi

java -ea                          \
  ${JAVA_OPTS}                    \
  -Xms512M                        \
  -Xmx1024M                       \
  -server                         \
  -jar ${DIR}/${jar} "$@"