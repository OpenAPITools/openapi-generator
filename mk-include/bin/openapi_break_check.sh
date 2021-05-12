#!/usr/bin/env bash

set -e

cleanup() {
  rm -rf $GIT_DIR
  rm -rf $OAD_DIR
}

# Make temp git repo from specified commit.
# If no commit specified, checkout master.
GIT_DIR=$(mktemp -d)
trap cleanup EXIT
git clone --depth=1 git@github.com:confluentinc/api.git $GIT_DIR
CWD=$(pwd)
cd $GIT_DIR
COMMIT=${2:-master}
git checkout $COMMIT

# Make temp git repo for openapi-diff.
OAD_DIR=$(mktemp -d)
git clone git@github.com:confluentinc/openapi-diff.git $OAD_DIR

# Install Maven?
cd $OAD_DIR
mvn package -Dmaven.javadoc.skip=true -Dmaven.test.skip=true -q
OAD_JAR=$OAD_DIR/target/openapi-diff-2.0.0-SNAPSHOT-jar-with-dependencies.jar

set +e
java -jar $OAD_JAR $GIT_DIR/$1 $CWD/$1 --markdown=oad.out --fail-on-incompatible
EXIT_CODE=$?
set -e
cat oad.out
if [[ $EXIT_CODE -ne 0 && -s "oad.out" && ! -z $SEMAPHORE_GIT_PR_NUMBER ]]; then
  BODY=$(cat oad.out | awk '{printf "%s\\n", $0}')
  curl -XPOST https://api.github.com/repos/confluentinc/api/issues/${SEMAPHORE_GIT_PR_NUMBER}/comments \
  -d '{"body": "'"$BODY"'"}' -n -H 'Content-type:application/json'
fi
