// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "TestClientTests",
    platforms: [
        .iOS(.v13),
        .macOS(.v10_15)
    ],
    products: [],
    dependencies: [.package(path: "../client/PetstoreOpenAPI")],
    targets: [
        .testTarget(
            name: "TestClientTests",
            dependencies: [.byName(name: "PetstoreOpenAPI")]
        ),
    ]
)
