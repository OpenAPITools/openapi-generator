# Specta

A light-weight TDD / BDD framework for Objective-C.

### Status
[![Build Status](https://travis-ci.org/specta/specta.png)](https://travis-ci.org/specta/specta)
[![Coverage Status](https://coveralls.io/repos/specta/specta/badge.svg)](https://coveralls.io/r/specta/specta)

## FEATURES

* An Objective-C RSpec-like BDD DSL
* Quick and easy set up
* Built on top of XCTest
* Excellent Xcode integration

## SCREENSHOT

![Specta Screenshot](https://raw.githubusercontent.com/specta/specta/master/misc/specta_screenshot.jpg)

## SETUP

Use [CocoaPods](http://github.com/CocoaPods/CocoaPods), [Carthage](https://github.com/carthage/carthage) or [Set up manually](#setting-up-manually)

### CocoaPods

1. Add Specta to your project's `Podfile`:

	```ruby
	target :MyApp do
	  # your app dependencies
	end

	target :MyAppTests do
	  pod 'Specta', '~> 0.5'
	  # pod 'Expecta',     '~> 0.3'   # expecta matchers
	  # pod 'OCMock',      '~> 2.2'   # OCMock
	  # pod 'OCHamcrest',  '~> 3.0'   # hamcrest matchers
	  # pod 'OCMockito',   '~> 1.0'   # OCMock
	  # pod 'LRMocky',     '~> 0.9'   # LRMocky
	end
	```

2. Run `pod update` or `pod install` in your project directory.

### Carthage

1. Add Specta to your project's `Cartfile.private`

    ```
    github "specta/specta" ~> 0.5
    ```

2. Run `carthage update` in your project directory
3. Drag the appropriate `Specta.framework` for your platform (located in Carthage/Build/) into your application’s Xcode project, and add it to your test target(s).
4. If you are building for iOS, a new `Run Script Phase` must be added to copy the framework. The instructions can be found on [Carthage's getting started instructions](https://github.com/carthage/carthage#getting-started)

### SETTING UP MANUALLY

1. Clone from Github.
2. Run `rake` in project root to build.
3. Add a "Cocoa/Cocoa Touch Unit Testing Bundle" target if you don't already have one.
4. Copy and add all header files in `Products` folder to the Test target in your Xcode project.
5. For **OS X projects**, copy and add `Specta.framework` in `Products/osx` folder to the test target in your Xcode project.
   For **iOS projects**, copy and add `Specta.framework` in `Products/ios` folder to the test target in your Xcode project.
   You can alternatively use `libSpecta.a`, if you prefer to add it as a static library for your project. (iOS 7 and below require this)
6. Add `-ObjC` and `-all_load` to the "Other Linker Flags" build setting for the test target in your Xcode project.
7. If you encounter linking issues with `_llvm_*` symbols, ensure your target's "Generate Test Coverage Files" and "Instrument Program Flow" build settings are set to `Yes`.

## EXAMPLE

```objective-c
#import <Specta/Specta.h> // #import "Specta.h" if you're using libSpecta.a

SharedExamplesBegin(MySharedExamples)
// Global shared examples are shared across all spec files.

sharedExamplesFor(@"foo", ^(NSDictionary *data) {
    __block id bar = nil;
    beforeEach(^{
        bar = data[@"bar"];
    });
    it(@"should not be nil", ^{
        XCTAssertNotNil(bar);
    });
});

SharedExamplesEnd

SpecBegin(Thing)

describe(@"Thing", ^{
  sharedExamplesFor(@"another shared behavior", ^(NSDictionary *data) {
    // Locally defined shared examples can override global shared examples within its scope.
  });

  beforeAll(^{
    // This is run once and only once before all of the examples
    // in this group and before any beforeEach blocks.
  });

  beforeEach(^{
    // This is run before each example.
  });

  it(@"should do stuff", ^{
    // This is an example block. Place your assertions here.
  });

  it(@"should do some stuff asynchronously", ^{
    waitUntil(^(DoneCallback done) {
      // Async example blocks need to invoke done() callback.
      done();
    });
  });

  itShouldBehaveLike(@"a shared behavior", @{@"key" : @"obj"});

  itShouldBehaveLike(@"another shared behavior", ^{
    // Use a block that returns a dictionary if you need the context to be evaluated lazily,
    // e.g. to use an object prepared in a beforeEach block.
    return @{@"key" : @"obj"};
  });

  describe(@"Nested examples", ^{
    it(@"should do even more stuff", ^{
      // ...
    });
  });

  pending(@"pending example");

  pending(@"another pending example", ^{
    // ...
  });

  afterEach(^{
    // This is run after each example.
  });

  afterAll(^{
    // This is run once and only once after all of the examples
    // in this group and after any afterEach blocks.
  });
});

SpecEnd
```

* `beforeEach` and `afterEach` are also aliased as `before` and `after` respectively.
* `describe` is also aliased as `context`.
* `it` is also aliased as `example` and `specify`.
* `itShouldBehaveLike` is also aliased as `itBehavesLike`.
* Use `pending` or prepend `x` to `describe`, `context`, `example`, `it`, and `specify` to mark examples or groups as pending.
* Use `^(DoneCallback done)` as shown in the example above to make examples wait for completion. `done()` callback needs to be invoked to let Specta know that your test is complete. The default timeout is 10.0 seconds but this can be changed by calling the function `setAsyncSpecTimeout(NSTimeInterval timeout)`.
* `(before|after)(Each/All)` also accept `^(DoneCallback done)`s.
* Do `#define SPT_CEDAR_SYNTAX` before importing Specta if you prefer to write `SPEC_BEGIN` and `SPEC_END` instead of `SpecBegin` and `SpecEnd`.
* Prepend `f` to your `describe`, `context`, `example`, `it`, and `specify` to set focus on examples or groups. When specs are focused, all unfocused specs are skipped.
* To use original XCTest reporter, set an environment variable named `SPECTA_REPORTER_CLASS` to `SPTXCTestReporter` in your test scheme.
* Set an environment variable `SPECTA_NO_SHUFFLE` with value `1` to disable test shuffling.
* Set an environment variable `SPECTA_SEED` to specify the random seed for test shuffling.

Standard XCTest matchers such as `XCTAssertEqualObjects` and `XCTAssertNil` work, but you probably want to add a nicer matcher framework - [Expecta](http://github.com/specta/expecta/) to your setup. Or if you really prefer, [OCHamcrest](https://github.com/jonreid/OCHamcrest) works fine too. Also, add a mocking framework: [OCMock](http://ocmock.org/).

## RUNNING TESTS IN COMMAND LINE

* Run `rake test` in the cloned folder.

## CONTRIBUTION GUIDELINES

* Please use only spaces and indent 2 spaces at a time.
* Please prefix instance variable names with a single underscore (`_`).
* Please prefix custom classes and functions defined in the global scope with `SPT`.

## LICENSE

Copyright (c) 2012-2015 [Specta Team](https://github.com/specta?tab=members). This software is licensed under the [MIT License](http://github.com/specta/specta/raw/master/LICENSE).
