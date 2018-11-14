#!/bin/sh

xcodebuild -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" test -destination "platform=iOS Simulator,name=iPhone 8,OS=12.1" | xcpretty && exit ${PIPESTATUS[0]}
