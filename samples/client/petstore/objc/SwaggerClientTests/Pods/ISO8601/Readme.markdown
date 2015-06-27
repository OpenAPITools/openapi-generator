# ISO8601

[![Version](https://img.shields.io/github/release/soffes/ISO8601.svg)](https://github.com/soffes/ISO8601/releases) [![Build Status](https://travis-ci.org/soffes/ISO8601.svg?branch=master)](https://travis-ci.org/soffes/ISO8601) [![Coverage Status](https://coveralls.io/repos/soffes/ISO8601/badge.svg?branch=master)](https://coveralls.io/r/soffes/ISO8601?branch=master) [![Carthage compatible](https://img.shields.io/badge/Carthage-compatible-4BC51D.svg?style=flat)](https://github.com/Carthage/Carthage) [![CocoaPods compatible](https://img.shields.io/cocoapods/v/ISO8601.svg)](https://cocoapods.org/pods/ISO8601)

Fast [ISO8601](http://en.wikipedia.org/wiki/ISO8601) date parser and writer for iOS & Mac.


## Installation

[Carthage](https://github.com/carthage/carthage) is the recommended way to install ISO8601. Add the following to your Cartfile:

``` ruby
github "soffes/ISO8601"
```

You can also install with [CocoaPods](https://cocoapods.org):

``` ruby
pod 'ISO8601'
```

For manual installation, I recommend adding the project as a subproject to your project or workspace and adding the framework as a target dependency.


## Usage

First, import the appropriate header:

``` objc
@import ISO8601; // Use #import <ISO8601/ISO8601.h> if you're using CocoaPods
```

This library uses `NSDateComponents` for reading and writing. Here's an example:

``` objc
// Reading
NSDateComponents *dateComponents = [ISO8601Serialization dateComponentsForString:@"1999-05-19T23:55:21+09:00"];

// Writing
NSString *ISO8601String = [ISO8601Serialization stringForDateComponents:dateComponents];
```

There is an `NSDate` category for convenient conversion:

``` objc
// Reading
NSDate *date = [NSDate dateWithISO8601String:@"1999-05-19T23:55:21+09:00"];

// Writing
NSString *ISO8601String = [date ISO8601String];
```

If you require more control over conversion or need to know the input time zone, you can use the advanced methods the category provides:


``` objc
// Reading
NSTimeZone *timeZone;
NSDate *date = [NSDate dateWithISO8601String:@"1999-05-19T23:55:21+09:00" timeZone:&timeZone usingCalendar:calendarOrNil];

// Writing
NSString *ISO8601String = [date ISO8601StringWithTimeZone:timeZoneOrNil usingCalendar:calendarOrNil];
```

## Notes

`NSDateComponents` is the core data structure because `NSDate` doesn't perserve time zone information well.

It's worth noting that a value in the `NSDateComponents` will be `nil` if it is not in the input string. For example, `1999-05-19T23:55:21` will have a `nil` time zone, but `1999-05-19T23:55:21+00:00` and `1999-05-19T23:55:21Z` will have a UTC time zone.

The `+[NSDate dateWithISO8601String:]` category will always return a UTC date. If you want a date in another time zone, you should use `+[NSDate ISO8601StringWithTimeZone:usingCalendar:]` (you may pass `nil` for the calendar parameter to use the current calendar).

Enjoy.
