#!/bin/sh

xcodebuild clean build build-for-testing -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 8,OS=12.4" && xcodebuild test-without-building -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 8,OS=12.4" | xcpretty && exit ${PIPESTATUS[0]}
