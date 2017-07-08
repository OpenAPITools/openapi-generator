![PromiseKit](http://promisekit.org/public/img/logo-tight.png)

![badge-pod] ![badge-languages] ![badge-pms] ![badge-platforms] ![badge-mit]

[繁體中文](README.zh_Hant.md) [简体中文](README.zh_CN.md)

---

Modern development is highly asynchronous: isn’t it about time we had tools that
made programming asynchronously powerful, easy and delightful?

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
    UIAlertView(/*…*/).show()
}
```

PromiseKit is a thoughtful and complete implementation of promises for any
platform with a `swiftc` (indeed, this includes *Linux*), it has *excellent* Objective-C bridging and
*delightful* specializations for iOS, macOS, tvOS and watchOS.

# Quick Start

We recommend [CocoaPods] or [Carthage], however you can just drop `PromiseKit.xcodeproj` into your project and add `PromiseKit.framework` to your app’s embedded frameworks.

## Xcode 8 / Swift 3

```ruby
# CocoaPods >= 1.1.0-rc.2
swift_version = "3.0"
pod "PromiseKit", "~> 4.0"

# Carthage
github "mxcl/PromiseKit" ~> 4.0

# SwiftPM
let package = Package(
    dependencies: [
        .Package(url: "https://github.com/mxcl/PromiseKit", majorVersion: 4)
    ]
)
```

## Xcode 8 / Swift 2.3 or Xcode 7

```ruby
# CocoaPods
swift_version = "2.3"
pod "PromiseKit", "~> 3.5"

# Carthage
github "mxcl/PromiseKit" ~> 3.5
```

# Documentation

We have thorough and complete documentation at [promisekit.org].

## Overview

Promises are defined by the function `then`:

```swift
login().then { json in
    //…
}
```

They are chainable:

```swift
login().then { json -> Promise<UIImage> in
    return fetchAvatar(json["username"])
}.then { avatarImage in
    self.imageView.image = avatarImage
}
```

Errors cascade through chains:

```swift
login().then {
    return fetchAvatar()
}.then { avatarImage in
    //…
}.catch { error in
    UIAlertView(/*…*/).show()
}
```

They are composable:

```swift
let username = login().then{ $0["username"] }

when(username, CLLocationManager.promise()).then { user, location in
    return fetchAvatar(user, location: location)
}.then { image in
    //…
}
```

They are trivial to refactor:

```swift
func avatar() -> Promise<UIImage> {
    let username = login().then{ $0["username"] }

    return when(username, CLLocationManager.promise()).then { user, location in
        return fetchAvatar(user, location: location)
    }
}
```

You can easily create a new, pending promise.
```swift
func fetchAvatar(user: String) -> Promise<UIImage> {
    return Promise { fulfill, reject in
        MyWebHelper.GET("\(user)/avatar") { data, err in
            guard let data = data else { return reject(err) }
            guard let img = UIImage(data: data) else { return reject(MyError.InvalidImage) }
            guard let img.size.width > 0 else { return reject(MyError.ImageTooSmall) }
            fulfill(img)
        }
    }
}
```

## Continue Learning…

Complete and progressive learning guide at [promisekit.org].

## PromiseKit vs. Xcode

PromiseKit contains Swift, so we engage in an unending battle with Xcode:

| Swift | Xcode | PromiseKit |   CI Status  |   Release Notes   |
| ----- | ----- | ---------- | ------------ | ----------------- |
|   3   |   8   |      4     | ![ci-master] | [2016/09][news-4] |
|   2   |  7/8  |      3     | ![ci-swift2] | [2015/10][news-3] |
|   1   |   7   |      3     |       –      | [2015/10][news-3] |
| *N/A* |   *   |      1†    | ![ci-legacy] |         –         |

† PromiseKit 1 is pure Objective-C and thus can be used with any Xcode, it is
also your only choice if you need to support iOS 7 or below.

---

We also maintain some branches to aid migrating between Swift versions:

| Xcode | Swift | PromiseKit | Branch                      | CI Status |
| ----- | ----- | -----------| --------------------------- | --------- |
|  8.0  |  2.3  | 2          | [swift-2.3-minimal-changes] | ![ci-23]  |
|  7.3  |  2.2  | 2          | [swift-2.2-minimal-changes] | ![ci-22]  |
|  7.2  |  2.2  | 2          | [swift-2.2-minimal-changes] | ![ci-22]  |
|  7.1  |  2.1  | 2          | [swift-2.0-minimal-changes] | ![ci-20]  |
|  7.0  |  2.0  | 2          | [swift-2.0-minimal-changes] | ![ci-20]  |

We do **not** usually backport fixes to these branches, but pull-requests are welcome.

# Extensions

Promises are only as useful as the asynchronous tasks they represent, thus we 
have converted (almost) all of Apple’s APIs to Promises. The default CocoaPod
comes with promises UIKit and Foundation, the rest are accessed by specifying
additional subspecs in your `Podfile`, eg:

```ruby
pod "PromiseKit/MapKit"        # MKDirections().promise().then { /*…*/ }
pod "PromiseKit/CoreLocation"  # CLLocationManager.promise().then { /*…*/ }
```

All our extensions are separate repositories at the [PromiseKit org ](https://github.com/PromiseKit).

For Carthage specify the additional repositories in your `Cartfile`:

```ruby
github "PromiseKit/MapKit" ~> 1.0
```

## Choose Your Networking Library

`NSURLSession` is typically inadequate; choose from [Alamofire] or [OMGHTTPURLRQ]:

```swift
// pod 'PromiseKit/Alamofire'  
Alamofire.request("http://example.com", withMethod: .GET).responseJSON().then { json in
    //…
}.catch { error in
    //…
}

// pod 'PromiseKit/OMGHTTPURLRQ'
URLSession.GET("http://example.com").asDictionary().then { json in
    
}.catch { error in
    //…
}
```

For [AFNetworking] we recommend [csotiriou/AFNetworking].


# Need to convert your codebase to Promises?

From experience it really improves the robustness of your app, feel free to ask us how to go about it.

# Support

Ask your question at our [Gitter chat channel](https://gitter.im/mxcl/PromiseKit) or on
[our bug tracker](https://github.com/mxcl/PromiseKit/issues/new).


[travis]: https://travis-ci.org/mxcl/PromiseKit
[ci-master]: https://travis-ci.org/mxcl/PromiseKit.svg?branch=master
[ci-legacy]: https://travis-ci.org/mxcl/PromiseKit.svg?branch=legacy-1.x
[ci-swift2]: https://travis-ci.org/mxcl/PromiseKit.svg?branch=swift-2.x
[ci-23]: https://travis-ci.org/mxcl/PromiseKit.svg?branch=swift-2.3-minimal-changes
[ci-22]: https://travis-ci.org/mxcl/PromiseKit.svg?branch=swift-2.2-minimal-changes
[ci-20]: https://travis-ci.org/mxcl/PromiseKit.svg?branch=swift-2.0-minimal-changes
[news-2]: http://promisekit.org/news/2015/05/PromiseKit-2.0-Released/
[news-3]: https://github.com/mxcl/PromiseKit/blob/master/CHANGELOG.markdown#300-oct-1st-2015
[news-4]: http://promisekit.org/news/2016/09/PromiseKit-4.0-Released/
[swift-2.3-minimal-changes]: https://github.com/mxcl/PromiseKit/tree/swift-2.3-minimal-changes
[swift-2.2-minimal-changes]: https://github.com/mxcl/PromiseKit/tree/swift-2.2-minimal-changes
[swift-2.0-minimal-changes]: https://github.com/mxcl/PromiseKit/tree/swift-2.0-minimal-changes
[promisekit.org]: http://promisekit.org/docs/
[badge-pod]: https://img.shields.io/cocoapods/v/PromiseKit.svg?label=version
[badge-platforms]: https://img.shields.io/badge/platforms-macOS%20%7C%20iOS%20%7C%20watchOS%20%7C%20tvOS-lightgrey.svg
[badge-languages]: https://img.shields.io/badge/languages-Swift%20%7C%20ObjC-orange.svg
[badge-mit]: https://img.shields.io/badge/license-MIT-blue.svg
[badge-pms]: https://img.shields.io/badge/supports-CocoaPods%20%7C%20Carthage%20%7C%20SwiftPM-green.svg
[OMGHTTPURLRQ]: https://github.com/mxcl/OMGHTTPURLRQ
[Alamofire]: http://alamofire.org
[AFNetworking]: https://github.com/AFNetworking/AFNetworking
[csotiriou/AFNetworking]: https://github.com/csotiriou/AFNetworking-PromiseKit
[CocoaPods]: http://cocoapods.org
[Carthage]: 2016-09-05-PromiseKit-4.0-Released
