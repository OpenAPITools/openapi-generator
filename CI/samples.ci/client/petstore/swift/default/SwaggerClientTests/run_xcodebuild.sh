#!/bin/sh

#pod install && xcodebuild clean test -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" -sdk iphonesimulator GCC_INSTRUMENT_PROGRAM_FLOW_ARCS=YES GCC_GENERATE_TEST_COVERAGE_FILES=YES | xcpretty

pod install && xcodebuild -workspace "SwaggerClient.xcworkspace" -scheme "SwaggerClient" test -destination "platform=iOS Simulator,name=iPhone 6,OS=9.3" | xcpretty
