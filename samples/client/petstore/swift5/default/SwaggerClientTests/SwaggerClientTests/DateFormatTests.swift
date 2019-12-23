//
//  DateFormatTests.swift
//  SwaggerClientTests
//
//  Created by James on 14/11/2018.
//  Copyright © 2018 Swagger. All rights reserved.
//

import Foundation
import XCTest
@testable import PetstoreClient
@testable import SwaggerClient

class DateFormatTests: XCTestCase {

	struct DateTest: Codable {
		let date: Date
	}

	override func setUp() {
		super.setUp()
		// Put setup code here. This method is called before the invocation of each test method in the class.
	}

	override func tearDown() {
		// Put teardown code here. This method is called after the invocation of each test method in the class.
		super.tearDown()
	}

	func testEncodeToJSONAlwaysResultsInUTCEncodedDate() {
		var dateComponents = DateComponents()
		dateComponents.calendar = Calendar(identifier: .gregorian)
		dateComponents.year = 2018
		dateComponents.month = 11
		dateComponents.day = 14
		dateComponents.hour = 11
		dateComponents.minute = 35
		dateComponents.second = 43
		dateComponents.nanosecond = 500

		// Testing a date with a timezone of +00:00 (UTC)
		dateComponents.timeZone = TimeZone(secondsFromGMT: 0)
		XCTAssert(dateComponents.isValidDate)

		guard let utcDate = dateComponents.date else {
			XCTFail("Couldn't get a valid date")
			return
		}

		var encodedDate = utcDate.encodeToJSON() as! String
		XCTAssert(encodedDate.hasSuffix("Z"))

		// test with a positive timzone offset from UTC
		dateComponents.timeZone = TimeZone(secondsFromGMT: 60 * 60) // +01:00
		XCTAssert(dateComponents.isValidDate)

		guard let nonUTCDate1 = dateComponents.date else {
			XCTFail("Couldn't get a valid date")
			return
		}

		encodedDate = nonUTCDate1.encodeToJSON() as! String
		XCTAssert(encodedDate.hasSuffix("Z"))

		// test with a negative timzone offset from UTC
		dateComponents.timeZone = TimeZone(secondsFromGMT: -(60 * 60)) // -01:00
		XCTAssert(dateComponents.isValidDate)

		guard let nonUTCDate2 = dateComponents.date else {
			XCTFail("Couldn't get a valid date")
			return
		}

		encodedDate = nonUTCDate2.encodeToJSON() as! String
		XCTAssert(encodedDate.hasSuffix("Z"))
	}

	func testCodableAlwaysResultsInUTCEncodedDate() throws {
        CodableHelper.jsonEncoder.outputFormatting.remove(.prettyPrinted)
		let jsonData = "{\"date\":\"1970-01-01T00:00:00.000Z\"}".data(using: .utf8)!
		let decodeResult = CodableHelper.decode(DateTest.self, from: jsonData)
        _ = try decodeResult.get()

		var dateComponents = DateComponents()
		dateComponents.calendar = Calendar(identifier: .gregorian)
		dateComponents.year = 1970
		dateComponents.month = 01
		dateComponents.day = 01
		dateComponents.hour = 00
		dateComponents.minute = 00
		dateComponents.second = 00

		// Testing a date with a timezone of +00:00 (UTC)
		dateComponents.timeZone = TimeZone(secondsFromGMT: 0)
		XCTAssert(dateComponents.isValidDate)

		guard let date = dateComponents.date else {
			XCTFail("Couldn't get a valid date")
			return
		}

		let dateTest = DateTest(date: date)
		let encodeResult = CodableHelper.encode(dateTest)
        let data = try encodeResult.get()
		guard let jsonString = String(data: data, encoding: .utf8) else {
			XCTFail("Unable to convert encoded data to string.")
			return
		}

		let exampleJSONString = "{\"date\":\"1970-01-01T00:00:00.000Z\"}"
		XCTAssert(jsonString == exampleJSONString, "Encoded JSON String: \(jsonString) should match: \(exampleJSONString)")
	}

}
