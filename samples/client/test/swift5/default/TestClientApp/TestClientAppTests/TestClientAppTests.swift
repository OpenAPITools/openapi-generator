//
//  TestClientAppTests.swift
//  TestClientAppTests
//
//  Created by Eric Hyche on 7/18/17.
//  Copyright © 2017 Swagger Codegen. All rights reserved.
//

import XCTest
import TestClient
@testable import TestClientApp

class TestClientAppTests: XCTestCase {

    func testWhenVariableNameIsDifferentFromPropertyName() throws {
        // This tests to make sure that the swift4 language can handle when
        // we have property names which map to variable names that are not the same.
        // This can happen when we have things like snake_case property names,
        // or when we have property names which may be Swift 4 reserved words.
        let jsonData = """
        {
           "example_name": "Test example name",
           "for": "Some reason",
           "normalName": "Some normal name value"
        }
        """.data(using: .utf8)!

        let decodedResult = CodableHelper.decode(VariableNameTest.self, from: jsonData)
        let variableNameTest = try decodedResult.get()

        XCTAssertTrue(variableNameTest.exampleName == "Test example name", "Did not decode snake_case property correctly.")
        XCTAssertTrue(variableNameTest._for == "Some reason", "Did not decode property name that is a reserved word correctly.")
    }

}
