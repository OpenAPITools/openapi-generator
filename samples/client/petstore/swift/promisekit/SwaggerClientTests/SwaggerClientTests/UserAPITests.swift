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
        let expectation = self.expectationWithDescription("testLogin")
        UserAPI.loginUser(username: "swiftTester", password: "swift").then { _ -> Void in
                expectation.fulfill()
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func testLogout() {
        let expectation = self.expectationWithDescription("testLogout")
        UserAPI.logoutUser().then {
            expectation.fulfill()
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func test1CreateUser() {
        let expectation = self.expectationWithDescription("testCreateUser")
        let newUser = User()
        newUser.email = "test@test.com"
        newUser.firstName = "Test"
        newUser.lastName = "Tester"
        newUser.id = 1000
        newUser.password = "test!"
        newUser.phone = "867-5309"
        newUser.username = "test@test.com"
        newUser.userStatus = 0
        UserAPI.createUser(body: newUser).then {
            expectation.fulfill()
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func test2GetUser() {
        let expectation = self.expectationWithDescription("testGetUser")
        UserAPI.getUserByName(username: "test@test.com").then {user -> Void in
                XCTAssert(user.userStatus == 0, "invalid userStatus")
                XCTAssert(user.email == "test@test.com", "invalid email")
                XCTAssert(user.firstName == "Test", "invalid firstName")
                XCTAssert(user.lastName == "Tester", "invalid lastName")
                XCTAssert(user.password == "test!", "invalid password")
                XCTAssert(user.phone == "867-5309", "invalid phone")
                expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
                XCTFail("error getting user")
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func test3DeleteUser() {
        let expectation = self.expectationWithDescription("testDeleteUser")
        UserAPI.deleteUser(username: "test@test.com").then {
            expectation.fulfill()
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

}
