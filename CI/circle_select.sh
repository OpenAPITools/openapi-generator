#!/bin/bash
# Only verified PR metadata may enable a skip. Uncertainty keeps the full node.
set -euo pipefail

node=${CIRCLE_NODE_INDEX:-0}
[[ "$node" =~ ^[0-3]$ ]] || { echo "Invalid CircleCI node: $node" >&2; exit 1; }
export CI_IS_PR=false
unset CI_BASE_SHA CI_HEAD_SHA
args=()
case "${CI_FORCE_FULL:-false}" in
  true|1) args+=(--force-full) ;;
  false|0) ;;
  *) echo "Invalid CI_FORCE_FULL" >&2; exit 1 ;;
esac

pr=
if [[ "${CIRCLE_PULL_REQUEST:-}" =~ ^https://github.com/OpenAPITools/openapi-generator/pull/([0-9]+)$ ]]; then
  pr=${BASH_REMATCH[1]}
elif [[ "${CIRCLE_BRANCH:-}" =~ ^pull/([0-9]+)$ ]]; then
  pr=${BASH_REMATCH[1]}
fi

if [ -n "$pr" ] && [ "${#args[@]}" -eq 0 ]; then
  # GitHub constructs this ref from the actual target branch, including release
  # branches and fork PRs. A moved/stale/unmergeable ref is not a usable baseline.
  if [[ "${CIRCLE_SHA1:-}" =~ ^[0-9a-fA-F]{40}$ ]] &&
     [ "$(git rev-parse HEAD)" = "$CIRCLE_SHA1" ] &&
     git fetch --no-tags --depth=2 origin "refs/pull/$pr/merge"; then
    parents=$(git cat-file -p FETCH_HEAD | sed -n '/^$/q; s/^parent //p')
    readarray -t parents <<< "$parents"
    if [ "${#parents[@]}" -eq 2 ] && [ "${parents[1]}" = "$CIRCLE_SHA1" ]; then
      export CI_IS_PR=true CI_BASE_SHA="${parents[0]}" CI_HEAD_SHA="$CIRCLE_SHA1"
    fi
  fi
  if [ "$CI_IS_PR" != true ]; then
    echo "CircleCI PR baseline could not be verified; running the full node." >&2
  fi
fi

python3 CI/select_changes.py --provider circleci --suite "circle.node$node" \
  --manifest CI/change-scopes.json --mode "${CI_SELECTION_MODE:-shadow}" \
  "${args[@]}" > .circleci-selection.json
has_changes=$(python3 - <<'PY'
import json
import os
import shlex
import sys

with open(".circleci-selection.json", encoding="utf-8") as stream:
    result = json.load(stream)
if (type(result.get("has_changes")) is not bool
        or type(result.get("run_all")) is not bool
        or not isinstance(result.get("matrix"), dict)
        or not isinstance(result.get("samples"), list)
        or any(not isinstance(sample, str) for sample in result["samples"])
        or not isinstance(result.get("reasons"), list)):
    raise ValueError("Invalid CircleCI selector output")
print(json.dumps(result), file=sys.stderr)
with open(os.environ["BASH_ENV"], "a", encoding="utf-8") as stream:
    stream.write("export CIRCLE_SELECTED_SAMPLES_JSON=" +
                 shlex.quote(json.dumps(result["samples"])) + "\n")
print("true" if result["has_changes"] else "false")
PY
)
if [ "$has_changes" = false ]; then
  echo "No relevant changes for circle.node$node; halting before setup."
  circleci-agent step halt
fi
