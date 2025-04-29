// swift-tools-version:6.0

import PackageDescription

let package = Package(
    name: "PetstoreClient",
    platforms: [
        .iOS(.v12),
        .macOS(.v10_13),
        .tvOS(.v12),
        .watchOS(.v4),
    ],
    products: [
        // Products define the executables and libraries produced by a package, and make them visible to other packages.
        .library(
            name: "PetstoreClient",
            targets: ["PetstoreClient"]
        ),
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        .package(url: "https://github.com/Alamofire/Alamofire", .upToNextMajor(from: "5.10.2")),
        .package(url: "https://github.com/mxcl/PromiseKit", .upToNextMajor(from: "8.1.2")),
        .package(url: "https://github.com/ReactiveX/RxSwift", .upToNextMajor(from: "6.8.0")),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages which this package depends on.
        .target(
            name: "PetstoreClient",
            dependencies: ["Alamofire", "PromiseKit", "RxSwift"],
            path: "Sources/PetstoreClient"
        ),
    ],
    swiftLanguageModes: [.v6]
)
