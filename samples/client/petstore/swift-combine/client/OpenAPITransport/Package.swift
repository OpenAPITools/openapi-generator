// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "OpenAPITransport",
    platforms: [
        .iOS(.v13),
        .macOS(.v10_15)
    ],
    products: [
        .library(
            name: "OpenAPITransport",
            targets: ["OpenAPITransport"]
        ),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "OpenAPITransport",
            dependencies: [],
            path: "Sources"
        ),
    ]
)
