#!/bin/sh

pod install && xcodebuild test -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient-Example" -destination "platform=iOS Simulator,name=iPhone 5s,OS=9.3" | xcpretty && exit ${PIPESTATUS[0]}
