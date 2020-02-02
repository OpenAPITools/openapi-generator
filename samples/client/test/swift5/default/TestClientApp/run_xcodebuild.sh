#!/bin/sh

pod install

xcodebuild clean build build-for-testing -workspace "TestClientApp.xcworkspace" -scheme "TestClientApp" -destination "platform=iOS Simulator,name=iPhone 11 Pro Max,OS=latest" && xcodebuild test-without-building -workspace "TestClientApp.xcworkspace" -scheme "TestClientAppTests" -destination "platform=iOS Simulator,name=iPhone 11 Pro Max,OS=latest" | xcpretty && exit ${PIPESTATUS[0]}
