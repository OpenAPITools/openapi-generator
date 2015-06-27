#Expecta

[![Build Status](http://img.shields.io/travis/specta/expecta/master.svg?style=flat)](https://travis-ci.org/specta/expecta)
[![Pod Version](http://img.shields.io/cocoapods/v/Expecta.svg?style=flat)](http://cocoadocs.org/docsets/Expecta/)
[![Pod Platform](http://img.shields.io/cocoapods/p/Expecta.svg?style=flat)](http://cocoadocs.org/docsets/Expecta/)
[![Pod License](http://img.shields.io/cocoapods/l/Expecta.svg?style=flat)](https://www.apache.org/licenses/LICENSE-2.0.html)

A matcher framework for Objective-C and Cocoa.

## Introduction

The main advantage of using Expecta over other matcher frameworks is that you do not have to specify the data types. Also, the syntax of Expecta matchers is much more readable and does not suffer from parenthesitis.

```objective-c
expect(@"foo").to.equal(@"foo"); // `to` is a syntactic sugar and can be safely omitted.
expect(foo).notTo.equal(1);
expect([bar isBar]).to.equal(YES);
expect(baz).to.equal(3.14159);
```

Expecta is framework-agnostic: it works well with XCTest and XCTest-compatible test frameworks such as [Specta](http://github.com/petejkim/specta/).


## Setup

You can setup Expecta using [Carthage](https://github.com/Carthage/Carthage), [CocoaPods](http://github.com/CocoaPods/CocoaPods) or [completely manually](#setting-up-manually).

### Carthage

1. Add Expecta to your project's `Cartfile.private`:

	```ruby
	github "specta/expecta" "master"
	```

2. Run `carthage update` in your project directory.
3. Drag the appropriate **Expecta.framework** for your platform (located in `Carthage/Build/`) into your application’s Xcode project, and add it to your test target(s).

### CocoaPods

1. Add Expecta to your project's `Podfile`:

	```ruby
	target :MyApp do
	# Your app's dependencies
	end

	target :MyAppTests do
	  pod 'Expecta', '~> 0.2.4'
	end
	```
	
2. Run `pod update` or `pod install` in your project directory.

### Setting Up Manually

1. Clone Expecta from Github.
2. Run `rake` in your project directory to build the frameworks and libraries.
3. Add a Cocoa or Cocoa Touch Unit Testing Bundle target to your Xcode project if you don't already have one.
4. For **OS X projects**, copy and add `Expecta.framework` in the `Products/osx` folder to your project's test target.

   For **iOS projects**, copy and add `Expecta.framework` in the `Products/ios` folder to your project's test target.
   
   You can also use `libExpecta.a` if you prefer to link Expecta as a static library — iOS 7.x and below require this.
   
6. Add `-ObjC` and `-all_load` to the **Other Linker Flags** build setting for the test target in your Xcode project.
7. You can now use Expecta in your test classes by adding the following import:

	```objective-c
	@import Expecta; // If you're using Expecta.framework
	
	// OR
	
	#import <Expecta/Expecta.h> // If you're using the static library, or the framework
	```

## Built-in Matchers

> `expect(x).to.equal(y);` compares objects or primitives x and y and passes if they are identical (==) or equivalent isEqual:).

> `expect(x).to.beIdenticalTo(y);` compares objects x and y and passes if they are identical and have the same memory address.

> `expect(x).to.beNil();` passes if x is nil.

> `expect(x).to.beTruthy();` passes if x evaluates to true (non-zero).

> `expect(x).to.beFalsy();` passes if x evaluates to false (zero).

> `expect(x).to.contain(y);` passes if an instance of NSArray or NSString x contains y.

> `expect(x).to.beSupersetOf(y);` passes if an instance of NSArray, NSSet, NSDictionary or NSOrderedSet x contains all elements of y.

> `expect(x).to.haveCountOf(y);` passes if an instance of NSArray, NSSet, NSDictionary or NSString x has a count or length of y.

> `expect(x).to.beEmpty();` passes if an instance of NSArray, NSSet, NSDictionary or NSString x has a count or length of .

> `expect(x).to.beInstanceOf([Foo class]);` passes if x is an instance of a class Foo.

> `expect(x).to.beKindOf([Foo class]);` passes if x is an instance of a class Foo or if x is an instance of any class that inherits from the class Foo.

> `expect([Foo class]).to.beSubclassOf([Bar class]);` passes if the class Foo is a subclass of the class Bar or if it is identical to the class Bar. Use beKindOf() for class clusters.

> `expect(x).to.beLessThan(y);` passes if `x` is less than `y`.

> `expect(x).to.beLessThanOrEqualTo(y);` passes if `x` is less than or equal to `y`.

> `expect(x).to.beGreaterThan(y);` passes if `x` is greater than `y`.

> `expect(x).to.beGreaterThanOrEqualTo(y);` passes if `x` is greater than or equal to `y`.

> `expect(x).to.beInTheRangeOf(y,z);` passes if `x` is in the range of `y` and `z`.

> `expect(x).to.beCloseTo(y);` passes if `x` is close to `y`.

> `expect(x).to.beCloseToWithin(y, z);` passes if `x` is close to `y` within `z`.

> `expect(^{ /* code */ }).to.raise(@"ExceptionName");` passes if a given block of code raises an exception named `ExceptionName`.

> `expect(^{ /* code */ }).to.raiseAny();` passes if a given block of code raises any exception.

> `expect(x).to.conformTo(y);` passes if `x` conforms to the protocol `y`.

> `expect(x).to.respondTo(y);` passes if `x` responds to the selector `y`.

> `expect(^{ /* code */ }).to.notify(@"NotificationName");` passes if a given block of code generates an NSNotification amed `NotificationName`.

> `expect(^{ /* code */ }).to.notify(notification);` passes if a given block of code generates an NSNotification equal to the passed `notification`.

> `expect(x).to.beginWith(y);` passes if an instance of NSString, NSArray, or NSOrderedSet `x` begins with `y`. Also liased by `startWith`

> `expect(x).to.endWith(y);` passes if an instance of NSString, NSArray, or NSOrderedSet `x` ends with `y`.

> `expect(x).to.match(y);` passes if an instance of NSString `x` matches regular expression (given as NSString) `y` one or more times.

## Inverting Matchers

Every matcher's criteria can be inverted by prepending `.notTo` or `.toNot`:

>`expect(x).notTo.equal(y);` compares objects or primitives x and y and passes if they are *not* equivalent.

## Asynchronous Testing

Every matcher can be made to perform asynchronous testing by prepending `.will`, `.willNot` or `after(...)`:

>`expect(x).will.beNil();` passes if x becomes nil before the default timeout.
>
>`expect(x).willNot.beNil();` passes if x becomes non-nil before the default timeout.
>
>`expect(x).after(3).to.beNil();` passes if x becoms nil after 3.0 seconds.
>
>`expect(x).after(2.5).notTo.equal(42);` passes if x doesn't equal 42 after 2.5 seconds.

The default timeout is 1.0 second and is used for all matchers if not otherwise specified. This setting can be changed by calling `[Expecta setAsynchronousTestTimeout:x]`, where `x` is the desired timeout in seconds.

```objective-c
describe(@"Foo", ^{
  beforeAll(^{
    // All asynchronous matching using `will` and `willNot`
    // will have a timeout of 2.0 seconds
    [Expecta setAsynchronousTestTimeout:2];
  });

  it(@"will not be nil", ^{
    // Test case where default timeout is used
    expect(foo).willNot.beNil();
  });

  it(@"should equal 42 after 3 seconds", ^{
    // Signle case where timeout differs from the default
    expect(foo).after(3).to.equal(42);
  });
});
```

## Forced Failing

You can fail a test by using the `failure` attribute. This can be used to test branching.

> `failure(@"This should not happen");` outright fails a test.


## Writing New Matchers

Writing a new matcher is easy with special macros provided by Expecta. Take a look at how `.beKindOf()` matcher is defined:

`EXPMatchers+beKindOf.h`

```objective-c
#import "Expecta.h"

EXPMatcherInterface(beKindOf, (Class expected));
// 1st argument is the name of the matcher function
// 2nd argument is the list of arguments that may be passed in the function
// call.
// Multiple arguments are fine. (e.g. (int foo, float bar))

#define beAKindOf beKindOf
```

`EXPMatchers+beKindOf.m`

```objective-c
#import "EXPMatchers+beKindOf.h"

EXPMatcherImplementationBegin(beKindOf, (Class expected)) {
  BOOL actualIsNil = (actual == nil);
  BOOL expectedIsNil = (expected == nil);

  prerequisite(^BOOL {
    return !(actualIsNil || expectedIsNil);
    // Return `NO` if matcher should fail whether or not the result is inverted
    // using `.Not`.
  });

  match(^BOOL {
    return [actual isKindOfClass:expected];
    // Return `YES` if the matcher should pass, `NO` if it should not.
    // The actual value/object is passed as `actual`.
    // Please note that primitive values will be wrapped in NSNumber/NSValue.
  });

  failureMessageForTo(^NSString * {
    if (actualIsNil)
      return @"the actual value is nil/null";
    if (expectedIsNil)
      return @"the expected value is nil/null";
    return [NSString
        stringWithFormat:@"expected: a kind of %@, "
                          "got: an instance of %@, which is not a kind of %@",
                         [expected class], [actual class], [expected class]];
    // Return the message to be displayed when the match function returns `YES`.
  });

  failureMessageForNotTo(^NSString * {
    if (actualIsNil)
      return @"the actual value is nil/null";
    if (expectedIsNil)
      return @"the expected value is nil/null";
    return [NSString
        stringWithFormat:@"expected: not a kind of %@, "
                          "got: an instance of %@, which is a kind of %@",
                         [expected class], [actual class], [expected class]];
    // Return the message to be displayed when the match function returns `NO`.
  });
}
EXPMatcherImplementationEnd
```

## Dynamic Predicate Matchers

It is possible to add predicate matchers by simply defining the matcher interface, with the matcher implementation being handled at runtime by delegating to the predicate method on your object.

For instance, if you have the following class:

```objc
@interface LightSwitch : NSObject
@property (nonatomic, assign, getter=isTurnedOn) BOOL turnedOn;
@end

@implementation LightSwitch
@synthesize turnedOn;
@end
```

The normal way to write an assertion that the switch is turned on would be:

```objc
expect([lightSwitch isTurnedOn]).to.beTruthy();
```

However, if we define a custom predicate matcher:

```objc
EXPMatcherInterface(isTurnedOn, (void));
```

(Note: we haven't defined the matcher implementation, just it's interface)

You can now write your assertion as follows:

```objc
expect(lightSwitch).isTurnedOn();
```

## Contribution Guidelines

* Please use only spaces and indent 2 spaces at a time.
* Please prefix instance variable names with a single underscore (`_`).
* Please prefix custom classes and functions defined in the global scope with `EXP`.

## License

Copyright (c) 2012-2015 [Specta Team](https://github.com/specta?tab=members). This software is licensed under the [MIT License](http://github.com/specta/specta/raw/master/LICENSE).
