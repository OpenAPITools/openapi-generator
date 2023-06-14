#!/bin/bash
set -e

(cd tests ; xcodebuild -scheme TestClientTests-Package test -destination "platform=iOS Simulator,name=iPhone 14,OS=latest" | xcpretty && exit ${PIPESTATUS[0]})

