#!/bin/sh

xcodebuild clean build-for-testing -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 16,OS=latest" && xcodebuild test-without-building -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 16,OS=latest" | xcpretty && exit ${PIPESTATUS[0]}
