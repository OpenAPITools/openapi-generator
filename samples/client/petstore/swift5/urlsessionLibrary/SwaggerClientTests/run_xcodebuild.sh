#!/bin/sh

pod install

xcodebuild clean build build-for-testing -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 8,OS=latest" && xcodebuild test-without-building -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 8,OS=latest" | xcpretty && exit ${PIPESTATUS[0]}
