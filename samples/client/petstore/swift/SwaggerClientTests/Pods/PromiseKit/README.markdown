![PromiseKit](http://methylblue.com/junk/PMKBanner.png)

Modern development is highly asynchronous: isn’t it about time we had tools that made programming asynchronously powerful, easy and delightful?

```swift
UIApplication.sharedApplication().networkActivityIndicatorVisible = true

when(fetchImage(), getLocation()).then { image, location in
    self.imageView.image = image;
    self.label.text = "Buy your cat a house in \(location)"
}.always {
    UIApplication.sharedApplication().networkActivityIndicatorVisible = false
}.error { error in
    UIAlertView(…).show()
}
```

PromiseKit is a thoughtful and complete implementation of promises for iOS and OS X with first-class support for **both** Objective-C *and* Swift.

[![Join the chat at https://gitter.im/mxcl/PromiseKit](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/mxcl/PromiseKit?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) ![](https://img.shields.io/cocoapods/v/PromiseKit.svg?label=Current%20Release)  [![Carthage compatible](https://img.shields.io/badge/Carthage-compatible-4BC51D.svg)](https://github.com/Carthage/Carthage)


# Which PromiseKit Should I Use?

If you are writing a library, **use PromiseKit 1.6**. This is because PromiseKit > 2 breaks everytime Swift changes. While Swift is in flux it is not feasible to depend on a library that will break every time Xcode updates.

If you are making an app then PromiseKit 3 is the best PromiseKit, you may have to make some fixes when Xcode updates, but probably you will be OK as long as you update PromiseKit when Xcode updates.

PromiseKit 1 and 3 can be installed in parallel if necessary, but CocoaPods will not support this.

Once Swift becomes ABI or API stable we can all just move to the latest PromiseKit.

Thus we intend to support PromiseKit 1.x for longer than expected.


# PromiseKit 3

In Swift 2.0 `catch` and `defer` became reserved keywords mandating we rename our functions with these names. This forced a major semantic version change on PromiseKit and thus we took the opportunity to make other minor (source compatability breaking) improvements.

Thus if you cannot afford to adapt to PromiseKit 3 but still want to use Xcode-7.0/Swift-2.0 we provide a [minimal changes branch] where `catch` and `defer` are renamed `catch_` and `defer_` and all other changes are the bare minimum to make PromiseKit 2 compile against Swift 2.

If you still are using Xcode 6 and Swift 1.2 then use PromiseKit 2.

[minimal changes branch]: https://github.com/mxcl/PromiseKit/tree/swift-2.0-minimal-changes

# PromiseKit 2

PromiseKit 2 contains many interesting and important additions. Check out our our [release announcement](http://promisekit.org/PromiseKit-2.0-Released/) for full details.

# PromiseKit 1

The original, nice to use with Objective-C, less nice to use with Swift, hence PromiseKit 2.


# How To Get Started

* Check out the complete, comprehensive [PromiseKit documentation](http://promisekit.org).
* Read the [API documentation](http://cocoadocs.org/docsets/PromiseKit/), (note the documentation is not 100% currently as CocoaDocs is not good with Swift, you may have better luck reading the comments in the sources).
* [Integrate](http://promisekit.org/getting-started) promises into your existing projects.

## Quick Start Guide

### CocoaPods

```ruby
use_frameworks!

pod "PromiseKit", "~> 2.0"
```

### Carthage
```ruby
github "mxcl/PromiseKit" ~> 2.0
```

*Note*: In order to avoid linking nearly all system frameworks with PromiseKit, the convenience categories have not been included with the Carthage framework . You must manually copy the categories you need in from the Carthage checkout.

### Standalone Distributions

* [iOS 8 & OS X 10.9  Frameworks](https://github.com/mxcl/PromiseKit/releases/download/2.2.2/PromiseKit-2.2.2.zip) (Binaries)

*Please note*, the preferred way to integrate PromiseKit is CocoaPods or Carthage.

###  iOS 7 And Below

Neither CocoaPods or Carthage will install PromiseKit 2 for an iOS 7 target. Your options are:

 1. `pod "PromiseKit", "~> 1.5"` †‡
 2. Use our [iOS 7 EZ-Bake](https://github.com/PromiseKit/EZiOS7)
 3. Download our pre-built static framework (coming soon!)

† There is no Swift support with PromiseKit 1.x installed via CocoaPods.<br>‡ PromiseKit 1.x will work as far back as iOS 5 if required.


# Donations

PromiseKit is hundreds of hours of work almost completely by just me: [Max Howell](https://twitter.com/mxcl). I thoroughly enjoyed making PromiseKit, but nevertheless if you have found it useful then your bitcoin will give me a warm fuzzy feeling from my head right down to my toes: 1JDbV5zuym3jFw4kBCc5Z758maUD8e4dKR.


# License

Copyright 2015, Max Howell; <mxcl@me.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
