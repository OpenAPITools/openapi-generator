#!/usr/bin/env bash
# this bash script generates all samples.
# it ensures that all changes are committed into the 'samples/' folder
# shellcheck disable=SC2155
declare cwd="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declare root="$(cd "$cwd" && cd ../ && pwd)"
declare executable="${root}/modules/openapi-generator-cli/target/openapi-generator-cli.jar"

echo "# START SCRIPT: $0"
echo "This script generates all configs under bin/configs by default."
echo "You may generate a targeted script or set of scripts using glob patterns."
echo "For example: $0 bin/configs/java-*"
echo ""
echo "Please press CTRL+C to stop or the script will continue in 5 seconds."

sleep 5
if [ ! -f "$executable" ]; then
  (cd "${root}" && mvn -B --no-snapshot-updates clean package -DskipTests=true -Dmaven.javadoc.skip=true -Djacoco.skip=true)
fi

export JAVA_OPTS="${JAVA_OPTS} -server -Duser.timezone=UTC"

configs=${@:-"${root}"/bin/configs/*.yaml}

# shellcheck disable=SC2086
java $JAVA_OPTS -jar "$executable" batch --includes-base-dir "${root}" --fail-fast  -- $configs
