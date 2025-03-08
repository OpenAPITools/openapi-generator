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
        // TODO: Alamofire versions 5.10.0 and above are not currently supported. If you need a newer version, please consider submitting a Pull Request with the required changes.
        .package(url: "https://github.com/Alamofire/Alamofire", exact: "5.9.1"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages which this package depends on.
        .target(
            name: "PetstoreClient",
            dependencies: ["Alamofire", ],
            path: "Sources/PetstoreClient"
        ),
    ],
    swiftLanguageModes: [.v6]
)
