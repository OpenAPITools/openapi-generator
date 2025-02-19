//
//  UserAPITests.swift
//  SwaggerClient
//
//  Created by Robin Eggenkamp on 5/21/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import XCTest
@testable import SwaggerClient

class UserAPITests: XCTestCase {

    let testTimeout = 10.0

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }

    func testLogin() {
        let expectation = self.expectation(description: "testLogin")

        PetstoreClientAPI.UserAPI.loginUser(username: "swiftTester", password: "swift") { (_, error) in
            guard error == nil else {
                XCTFail("error logging in")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func testLogout() {
        let expectation = self.expectation(description: "testLogout")

        PetstoreClientAPI.UserAPI.logoutUser { (_, error) in
            guard error == nil else {
                XCTFail("error logging out")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func testPathParamsAreEscaped() {
        // The path for this operation is /user/{userId}. In order to make a usable path,
        // then we must make sure that {userId} is percent-escaped when it is substituted
        // into the path. So we intentionally introduce a path with spaces.
        let userRequestBuilder = PetstoreClientAPI.UserAPI.getUserByNameWithRequestBuilder(username: "User Name With Spaces")
        let urlContainsSpace = userRequestBuilder.URLString.contains(" ")

        XCTAssert(!urlContainsSpace, "Expected URL to be escaped, but it was not.")
    }

}
