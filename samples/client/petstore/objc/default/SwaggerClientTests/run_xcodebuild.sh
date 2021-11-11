#!/bin/sh

pod install && xcodebuild test -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient-Example" -destination "platform=iOS Simulator,name=iPhone 8,OS=13.1" | xcpretty && exit ${PIPESTATUS[0]}
