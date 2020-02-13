#!/bin/sh

pod install

xcodebuild -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" test -destination "platform=iOS Simulator,name=iPhone 8,OS=latest" | xcpretty && exit ${PIPESTATUS[0]}
