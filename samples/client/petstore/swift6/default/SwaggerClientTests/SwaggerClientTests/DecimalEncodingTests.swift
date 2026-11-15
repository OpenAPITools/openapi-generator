import Foundation
import XCTest
@testable import PetstoreClient

final class DecimalEncodingTests: XCTestCase {

    private struct Amount: Encodable {
        let value: Decimal

        enum CodingKeys: String, CodingKey {
            case value
        }

        func encode(to encoder: Encoder) throws {
            var container = encoder.container(keyedBy: CodingKeys.self)
            try container.encode(value, forKey: .value)
        }
    }

    func testDecimalIsEncodedExactly() throws {
        let cases: [(input: String, expected: String)] = [
            ("12.34567", "12.34567"),
            ("123.456789", "123.456789"),
            ("1500.22", "1500.22"),
            ("-0.0001", "-0.0001"),
            ("0.0000001", "0.0000001"),
            ("123456789012345678901234567890", "123456789012345678901234567890"),
            ("42", "42"),
        ]
        for (input, expected) in cases {
            let decimal = try XCTUnwrap(Decimal(string: input, locale: Locale(identifier: "en_US_POSIX")))
            let data = try JSONEncoder().encode(Amount(value: decimal))
            XCTAssertEqual(String(data: data, encoding: .utf8), "{\"value\":\"\(expected)\"}")
        }
    }
}
