#!/bin/sh

pod install && xcodebuild clean build build-for-testing -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient-Example" -destination "platform=iOS Simulator,name=iPhone 5s,OS=9.3" && xcodebuild test-without-building -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient-Example" -destination "platform=iOS Simulator,name=iPhone 5s,OS=9.3" | xcpretty && exit ${PIPESTATUS[0]}
