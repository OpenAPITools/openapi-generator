//
//  UserAPITests.swift
//  SwaggerClient
//
//  Created by Joseph Zuromski on 2/8/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import PromiseKit
import XCTest
@testable import SwaggerClient

class UserAPITests: XCTestCase {

    let testTimeout = 10.0

    func testLogin() {
        let expectation = self.expectation(description: "testLogin")
        UserAPI.loginUser(username: "swiftTester", password: "swift").then { _ -> Void in
            expectation.fulfill()
            }.always {
                // Noop for now
            }.catch { (_) in
                XCTFail("login error")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func testLogout() {
        let expectation = self.expectation(description: "testLogout")
        UserAPI.logoutUser().then {
            expectation.fulfill()
            }.always {
                // Noop for now
            }.catch { (_) in
                XCTFail("")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
}
