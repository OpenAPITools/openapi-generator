#!/bin/bash
set -e

DIRECTORY=`dirname $0`

(cd $DIRECTORY/tests ; xcodebuild -scheme TestClientTests-Package test -destination "platform=iOS Simulator,name=iPhone 14,OS=latest" | xcpretty && exit ${PIPESTATUS[0]})

