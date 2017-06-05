#!/bin/sh

pod install && xcodebuild -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" test -destination "platform=iOS Simulator,name=iPhone 6,OS=9.3" | xcpretty && exit ${PIPESTATUS[0]}
