![PromiseKit](http://promisekit.org/public/img/logo-tight.png)

![badge-pod] ![badge-languages] ![badge-pms] ![badge-platforms] [![Build Status](https://travis-ci.org/mxcl/PromiseKit.svg?branch=master)](https://travis-ci.org/mxcl/PromiseKit)

[繁體中文](README.zh_Hant.md), [简体中文](README.zh_CN.md)

---

Promises simplify asynchronous programming, freeing you up to focus on the more
important things. They are easy to learn, easy to master and result in clearer,
more readable code. Your co-workers will thank you.

```swift
UIApplication.shared.isNetworkActivityIndicatorVisible = true

firstly {
    when(URLSession.dataTask(with: url).asImage(), CLLocationManager.promise())
}.then { image, location -> Void in
    self.imageView.image = image
    self.label.text = "\(location)"
}.always {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
}.catch { error in
    self.show(UIAlertController(for: error), sender: self)
}
```

PromiseKit is a thoughtful and complete implementation of promises for any
platform with a `swiftc`, it has *excellent* Objective-C bridging and
*delightful* specializations for iOS, macOS, tvOS and watchOS.

# Quick Start

In your [Podfile]:

```ruby
use_frameworks!

target "Change Me!" do
  pod "PromiseKit", "~> 4.4"
end
```

PromiseKit 4 supports Xcode 8.1, 8.2, 8.3 and 9.0; Swift 3.0, 3.1, 3.2 and 4.0; iOS, macOS, tvOS, watchOS, Linux and Android; CocoaPods, Carthage and SwiftPM; ([CI Matrix](https://travis-ci.org/mxcl/PromiseKit)).

For Carthage, SwiftPM, etc., or for instructions when using older Swifts or
Xcodes see our [Installation Guide](Documentation/Installation.md).

# Documentation

* Handbook
  * [Getting Started](Documentation/GettingStarted.md)
  * [Promises: Common Patterns](Documentation/CommonPatterns.md)
  * [Frequently Asked Questions](Documentation/FAQ.md)
* Manual
  * [Installation Guide](Documentation/Installation.md)
  * [Objective-C Guide](Documentation/ObjectiveC.md)
  * [Troubleshooting](Documentation/Troubleshooting.md) (eg. solutions to common compile errors)
  * [Appendix](Documentation/Appendix.md)

If you are looking for a function’s documentation, then please note
[our sources](Sources/) are thoroughly documented.

# Extensions

Promises are only as useful as the asynchronous tasks they represent, thus we
have converted (almost) all of Apple’s APIs to promises. The default CocoaPod
comes with promises for UIKit and Foundation, the rest can be installed by
specifying additional subspecs in your `Podfile`, eg:

```ruby
pod "PromiseKit/MapKit"          # MKDirections().promise().then { /*…*/ }
pod "PromiseKit/CoreLocation"    # CLLocationManager.promise().then { /*…*/ }
```

All our extensions are separate repositories at the [PromiseKit organization].

## Choose Your Networking Library

Promise chains are commonly started with networking, thus we offer multiple
options: [Alamofire], [OMGHTTPURLRQ] and of course (vanilla) `NSURLSession`:

```swift
// pod 'PromiseKit/Alamofire'
// https://github.com/PromiseKit/Alamofire
Alamofire.request("http://example.com", method: .post).responseJSON().then { json in
    //…
}.catch { error in
    //…
}

// pod 'PromiseKit/OMGHTTPURLRQ'
// https://github.com/PromiseKit/OMGHTTPURLRQ
URLSession.POST("http://example.com").asDictionary().then { json in
    //…
}.catch { error in
    //…
}

// pod 'PromiseKit/Foundation'
// https://github.com/PromiseKit/Foundation
URLSession.shared.dataTask(url).asDictionary().then { json in
    // …
}.catch { error in
    //…
}
```

Nobody ever got fired for using Alamofire, but at the end of the day, it’s
just a small wrapper around `NSURLSession`. OMGHTTPURLRQ supplements
`NSURLRequest` to make generating REST style requests easier, and the PromiseKit
extensions extend `NSURLSession` to make OMG usage more convenient. But since a
while now most servers accept JSON, so writing a simple API class that uses
vanilla `NSURLSession` and our promises is not hard, and gives you the most
control with the fewest black-boxes.

The choice is yours.

# Support

Ask your question at our [Gitter chat channel] or on [our bug tracker].


[badge-pod]: https://img.shields.io/cocoapods/v/PromiseKit.svg?label=version
[badge-pms]: https://img.shields.io/badge/supports-CocoaPods%20%7C%20Carthage%20%7C%20SwiftPM-green.svg
[badge-languages]: https://img.shields.io/badge/languages-Swift%20%7C%20ObjC-orange.svg
[badge-platforms]: https://img.shields.io/badge/platforms-macOS%20%7C%20iOS%20%7C%20watchOS%20%7C%20tvOS%20%7C%20Linux-lightgrey.svg
[badge-mit]: https://img.shields.io/badge/license-MIT-blue.svg
[OMGHTTPURLRQ]: https://github.com/mxcl/OMGHTTPURLRQ
[Alamofire]: http://alamofire.org
[PromiseKit organization]: https://github.com/PromiseKit
[Gitter chat channel]: https://gitter.im/mxcl/PromiseKit
[our bug tracker]: https://github.com/mxcl/PromiseKit/issues/new
[Podfile]: https://guides.cocoapods.org/syntax/podfile.html
