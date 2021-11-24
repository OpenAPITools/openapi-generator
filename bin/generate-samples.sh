#!/usr/bin/env bash
# this bash script generates all samples.
# it ensures that all changes are committed into the 'samples/' folder
# shellcheck disable=SC2155
declare cwd="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declare root="$(cd "$cwd" && cd ../ && pwd)"
declare executable="${root}/modules/openapi-generator-cli/target/openapi-generator-cli.jar"

if [ ! -f "$executable" ]; then
  (cd "${root}" && mvn -B --no-snapshot-updates clean package -DskipTests=true -Dmaven.javadoc.skip=true -Djacoco.skip=true)
fi

export JAVA_OPTS="${JAVA_OPTS} -ea -server -Duser.timezone=UTC"
export BATCH_OPTS="${BATCH_OPTS:-}"

files=()
args=()
end_option=false
while [[ $# -gt 0 ]]; do
  key="$1"
  if [ "--" == "$key" ]; then
    end_option=true
  else
    if [[ "$end_option" = true ]]; then
      args+=("$1")
    else
      files+=("$1")
    fi
  fi
  shift
done

header="# START SCRIPT: $0
This script generates all configs under bin/configs by default.
You may generate a targeted script or set of scripts using glob patterns.

For example:
    $0 bin/configs/java-*

You may generate a single config with additional options if you use -- to
separate the single config file from the generator arguments.

For example:
    $0 bin/configs/java-vertx.yaml -- --global-property debugModels=true

"

echo "$header"

if [[ ${#files[@]} -eq 1 && "${files[0]}" != *'*'* ]]; then
    # shellcheck disable=SC2086
    # shellcheck disable=SC2068
    java ${JAVA_OPTS} -jar "$executable" generate -c ${files[0]} ${args[@]}
else
    echo "Please press CTRL+C to stop or the script will continue in 5 seconds."

    sleep 5

    if [ ${#files[@]} -eq 0 ]; then
      files=("${root}"/bin/configs/*.yaml)
    fi

    # shellcheck disable=SC2086
    # shellcheck disable=SC2068
    java ${JAVA_OPTS} -jar "$executable" batch ${BATCH_OPTS} --includes-base-dir "${root}" --fail-fast  -- ${files[@]}
fi

