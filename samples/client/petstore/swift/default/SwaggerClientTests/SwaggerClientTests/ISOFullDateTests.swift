/// Copyright 2012-2016 (C) Butterfly Network, Inc.

import PetstoreClient
import XCTest
@testable import SwaggerClient

final class ISOFullDateTests: XCTestCase {

    var fullDate = ISOFullDate(year: 1999, month: 12, day: 31)

    func testValidDate() {
        XCTAssertEqual(fullDate.year, 1999)
        XCTAssertEqual(fullDate.month, 12)
        XCTAssertEqual(fullDate.day, 31)
    }

    func testFromDate() {
        let date = NSDate()
        let fullDate = ISOFullDate.from(date: date)

        guard let calendar = NSCalendar(identifier: NSCalendarIdentifierGregorian) else {
            XCTFail()
            return
        }

        let components = calendar.components(
            [
                .Year,
                .Month,
                .Day
            ],
            fromDate: date
        )

        XCTAssertEqual(fullDate?.year, components.year)
        XCTAssertEqual(fullDate?.month, components.month)
        XCTAssertEqual(fullDate?.day, components.day)
    }

    func testFromString() {
        let string = "1999-12-31"
        let fullDate = ISOFullDate.from(string: string)
        XCTAssertEqual(fullDate?.year, 1999)
        XCTAssertEqual(fullDate?.month, 12)
        XCTAssertEqual(fullDate?.day, 31)
    }

    func testFromInvalidString() {
        XCTAssertNil(ISOFullDate.from(string: "1999-12"))
    }

    func testToDate() {
        let fullDate = ISOFullDate(year: 1999, month: 12, day: 31)

        guard let date = fullDate.toDate(),
              let calendar = NSCalendar(identifier: NSCalendarIdentifierGregorian) else {
            XCTFail()
            return
        }

        let components = calendar.components(
            [
                .Year,
                .Month,
                .Day
            ],
            fromDate: date
        )

        XCTAssertEqual(fullDate.year, components.year)
        XCTAssertEqual(fullDate.month, components.month)
        XCTAssertEqual(fullDate.day, components.day)
    }

    func testDescription() {
        XCTAssertEqual(fullDate.description, "1999-12-31")
    }

    func testEncodeToJSON() {
        XCTAssertEqual(fullDate.encodeToJSON() as? String, "1999-12-31")
    }

}
