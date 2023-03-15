//
//  APIHelperTests.swift
//  SwaggerClientTests
//
//  Created by Daiki Matsudate on 2018/03/12.
//  Copyright © 2018 Swagger. All rights reserved.
//

import XCTest
import PetstoreClient
@testable import SwaggerClient

class APIHelperTests: XCTestCase {

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }

    func testRejectNil() {
        let source: [String: Any?] = ["a": 1, "b": nil, "c": ["1", nil, "2"], "d": true, "e": false]
        let expected: [String: Any] = ["a": 1, "c": ["1", nil, "2"], "d": true, "e": false]
        let actual: [String: Any] = APIHelper.rejectNil(source)!
        XCTAssert(NSDictionary(dictionary: actual).isEqual(to: expected))
    }

    func testRejectNilHeaders() {
        let source: [String: Any?] = ["a": 1, "b": nil, "c": ["1", nil, "2"], "d": true, "e": false]
        let expected: [String: String] = ["a": "1", "c": "1,2", "d": "true", "e": "false"]
        let actual: [String: String] = APIHelper.rejectNilHeaders(source)
        XCTAssert(NSDictionary(dictionary: actual).isEqual(to: expected))
    }

    func testConvertBoolToString() {
        let source: [String: Any] = ["a": 1, "c": ["1", nil, "2"], "d": true, "e": false]
        let expected: [String: Any] = ["a": 1, "c": ["1", nil, "2"], "d": "true", "e": "false"]
        let actual: [String: Any] = APIHelper.convertBoolToString(source)!
        XCTAssert(NSDictionary(dictionary: actual).isEqual(to: expected))
    }

    func testMapValuesToQueryItems() {
        let source: [String: Any] = ["a": 1, "c": ["1", nil, "2"], "d": true, "e": false]
        let expected: [URLQueryItem] = [URLQueryItem(name: "a", value: "1"),
                                      URLQueryItem(name: "c", value: "1,2"),
                                      URLQueryItem(name: "d", value: "true"),
                                      URLQueryItem(name: "e", value: "false")].sorted(by: { $0.name > $1.name })
        let actual: [URLQueryItem] = APIHelper.mapValuesToQueryItems(source)!.sorted(by: { $0.name > $1.name })
        XCTAssert(actual == expected)
    }
}
