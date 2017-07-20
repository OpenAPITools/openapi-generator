#!/bin/sh

xcodebuild clean build build-for-testing -workspace "TestClientApp.xcworkspace" -scheme "TestClient" -destination "platform=iOS Simulator,name=iPhone 6,OS=9.3" && xcodebuild test-without-building -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -destination "platform=iOS Simulator,name=iPhone 6,OS=9.3" | xcpretty && exit ${PIPESTATUS[0]}
