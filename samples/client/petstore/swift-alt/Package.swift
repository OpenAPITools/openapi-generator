// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "OpenAPIClient",
    platforms: [
        .iOS(.v13)
    ],
    products: [
        .library(
            name: "OpenAPIClient",
            targets: ["OpenAPIClient"]
        ),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "OpenAPIClient",
            dependencies: [],
            path: "Sources"
        ),
    ]
)
