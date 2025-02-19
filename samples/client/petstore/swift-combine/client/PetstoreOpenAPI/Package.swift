// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "PetstoreOpenAPI",
    platforms: [
        .iOS(.v13),
        .macOS(.v10_15)
    ],
    products: [
        .library(
            name: "PetstoreOpenAPI",
            targets: ["PetstoreOpenAPI"]
        ),
    ],
    dependencies: [.package(path: "../OpenAPITransport")],
    targets: [
        .target(
            name: "PetstoreOpenAPI",
            dependencies: [.byName(name: "OpenAPITransport")],
            path: "Sources"
        ),
    ]
)
