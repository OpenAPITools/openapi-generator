#!/usr/bin/env bash
# Runs a build command and retries it when it failed because of a transient network error
# (rate limiting, 5xx, timeouts) while downloading dependencies. Any other failure, such as
# a compilation error, is reported immediately without retrying.
#
# If a failed attempt involved the Maven Central mirror (see
# .github/gradle/maven-central-mirror.init.gradle), the next attempt runs without it, so an
# outage of the mirror falls back to the build's original repositories.
#
# Usage: retry-on-network-error.sh <command> [args...]
#
# Environment:
#   RETRY_MAX_ATTEMPTS    total number of attempts (default: 3)
#   RETRY_INITIAL_DELAY   base seconds to wait before the first retry, doubled each time
#                         (default: 30); a random 0-100% is added on top so that jobs which
#                         hit a rate limit together do not all retry at the same moment
#   MAVEN_CENTRAL_MIRROR  mirror URL in use, if any (exported by setup-maven-central-mirror.sh)
set -uo pipefail

if [ "$#" -eq 0 ]; then
  echo "Usage: $0 <command> [args...]" >&2
  exit 2
fi

max_attempts=${RETRY_MAX_ATTEMPTS:-3}
delay=${RETRY_INITIAL_DELAY:-30}

# Gradle:  "Received status code 429 from server", "Could not GET '...'", "Read timed out"
# Maven:   "status code: 429", "Could not transfer artifact", "Connection reset"
network_error='status code:? (429|5[0-9][0-9])|Could not (GET|HEAD) |Could not transfer (artifact|metadata)|Connection reset|Connection refused|Read timed out|Connect(ion)? timed out|Remote host terminated the handshake|Premature end of Content-Length'

# Matches the mirror's repository name (used in "Searched in the following repositories")
# or its URL (used in "Could not GET '...'").
mirror_used() {
  grep -qF 'MavenCentralMirror' "$1" && return 0
  [ -n "${MAVEN_CENTRAL_MIRROR:-}" ] && grep -qF "${MAVEN_CENTRAL_MIRROR%/}" "$1"
}

log=$(mktemp)
trap 'rm -f "$log"' EXIT

for attempt in $(seq 1 "$max_attempts"); do
  "$@" 2>&1 | tee "$log"
  status=${PIPESTATUS[0]}
  if [ "$status" -eq 0 ]; then
    exit 0
  fi
  if [ "$attempt" -ge "$max_attempts" ]; then
    exit "$status"
  fi

  if [ "${MAVEN_CENTRAL_MIRROR-unset}" != "" ] && mirror_used "$log"; then
    # Read by maven-central-mirror.init.gradle: an empty value disables the mirror.
    export MAVEN_CENTRAL_MIRROR=""
    echo "::warning::Dependency resolution via the Maven Central mirror failed (attempt $attempt/$max_attempts), retrying without the mirror"
  elif grep -qE "$network_error" "$log"; then
    pause=$((delay + RANDOM % (delay + 1)))
    echo "::warning::Network error while running '$*' (attempt $attempt/$max_attempts), retrying in ${pause}s"
    sleep "$pause"
    delay=$((delay * 2))
  else
    exit "$status"
  fi
done
