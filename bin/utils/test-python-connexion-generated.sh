#!/bin/bash
#
# Generate the 2 test specs with the 4 Connexion-related generators
# (python-flask, python-flask with useConnexion3, python-aiohttp,
# python-blueplanet) and check each output with
# check-python-connexion-generated.py.
#
# Prerequisite: modules/openapi-generator-cli/target/openapi-generator-cli.jar
# must already be built.
# Usage: test-python-connexion-generated.sh [output_dir]   (default: mktemp -d)

set -euo pipefail

cd "$(dirname "$0")/../.."

executable="modules/openapi-generator-cli/target/openapi-generator-cli.jar"
if [ ! -f "$executable" ]; then
    echo "ERROR: $executable not found. Build it first with:" >&2
    echo "  ./mvnw -B -ntp -DskipTests -pl modules/openapi-generator-cli -am package" >&2
    exit 1
fi

out_dir="${1:-$(mktemp -d)}"
specs=(
  modules/openapi-generator/src/test/resources/3_0/python/connexion-json-header-test-values.yaml
  modules/openapi-generator/src/test/resources/3_0/python/info-title-special-chars.yaml
)
variants=(
  "python-flask"
  "python-flask useConnexion3=true"
  "python-aiohttp"
  "python-blueplanet"
)
for spec in "${specs[@]}"; do
  for variant in "${variants[@]}"; do
    read -r generator option <<< "$variant"
    dest="$out_dir/$(basename "$spec" .yaml)/${generator}${option:++connexion3}"
    args=(generate -g "$generator" -i "$spec" -o "$dest")
    if [ -n "${option:-}" ]; then
      args+=(--additional-properties "$option")
    fi
    java -jar "$executable" "${args[@]}"
    python3 bin/utils/check-python-connexion-generated.py "$dest" \
      --check-tests --check-setup "$spec"
  done
done
