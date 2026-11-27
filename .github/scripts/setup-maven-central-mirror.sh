#!/usr/bin/env bash
# Enables the Maven Central mirror for Gradle builds in this job by installing
# .github/gradle/maven-central-mirror.init.gradle into the Gradle User Home.
#
# The mirror is probed first; if it does not respond, it is skipped and builds use their
# original repositories. Run it after gradle/actions/setup-gradle, which restores the Gradle
# User Home and skips that if the directory already exists.
#
# Environment:
#   MAVEN_CENTRAL_MIRROR  mirror URL (default: Google's mirror); empty disables the mirror
set -euo pipefail

mirror=${MAVEN_CENTRAL_MIRROR-https://maven-central.storage-download.googleapis.com/maven2/}
repo_root=$(cd "$(dirname "$0")/../.." && pwd)
gradle_home=${GRADLE_USER_HOME:-$HOME/.gradle}
github_env=${GITHUB_ENV:-/dev/null}

if [ -z "$mirror" ]; then
  echo "Maven Central mirror disabled (MAVEN_CENTRAL_MIRROR is empty)"
  exit 0
fi

# Any small, long-lived artifact will do.
probe="${mirror%/}/junit/junit/4.13.2/junit-4.13.2.pom"
if curl --fail --silent --show-error --location --max-time 15 \
    --retry 2 --retry-delay 5 --retry-all-errors --output /dev/null "$probe"; then
  mkdir -p "$gradle_home/init.d"
  cp "$repo_root/.github/gradle/maven-central-mirror.init.gradle" "$gradle_home/init.d/"
  echo "MAVEN_CENTRAL_MIRROR=$mirror" >> "$github_env"
  echo "Using Maven Central mirror $mirror"
else
  echo "MAVEN_CENTRAL_MIRROR=" >> "$github_env"
  echo "::warning::Maven Central mirror $mirror is not responding, using the original repositories"
fi
