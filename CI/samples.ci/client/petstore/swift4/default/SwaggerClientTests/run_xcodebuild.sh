#!/bin/sh

xcodebuild clean build build-for-testing -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 8,OS=13.0" && xcodebuild test-without-building -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 8,OS=13.0" | xcpretty && exit ${PIPESTATUS[0]}
