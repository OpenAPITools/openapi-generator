#!/bin/sh

(cd $DIRECTORY/tests ; xcodebuild -scheme TestClientTests-Package test -destination "platform=iOS Simulator,name=iPhone 17,OS=latest" | xcpretty && exit ${PIPESTATUS[0]})
