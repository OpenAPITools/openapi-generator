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
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func testLogout() {
        let expectation = self.expectation(description: "testLogout")
        UserAPI.logoutUser().then {
            expectation.fulfill()
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test1CreateUser() {
        let expectation = self.expectation(description: "testCreateUser")
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
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func testCreateUserWithArray() {
        let expectation = self.expectation(description: "testCreateUserWithArray")
        let newUser = User()
        newUser.email = "test@test.com"
        newUser.firstName = "Test"
        newUser.lastName = "Tester"
        newUser.id = 1000
        newUser.password = "test!"
        newUser.phone = "867-5309"
        newUser.username = "test@test.com"
        newUser.userStatus = 0
        
        let newUser2 = User()
        newUser2.email = "test2@test.com"
        newUser2.firstName = "Test2"
        newUser2.lastName = "Tester2"
        newUser2.id = 1001
        newUser2.password = "test2!"
        newUser2.phone = "867-5302"
        newUser2.username = "test2@test.com"
        newUser2.userStatus = 0
        
        _ = UserAPI.createUsersWithArrayInput(body: [newUser, newUser2]).then {
            expectation.fulfill()
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test2GetUser() {
        let expectation = self.expectation(description: "testGetUser")
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
            }.catch { errorType in
                XCTFail("error getting user")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test3DeleteUser() {
        let expectation = self.expectation(description: "testDeleteUser")
        UserAPI.deleteUser(username: "test@test.com").then {
            expectation.fulfill()
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func testPathParamsAreEscaped() {
        // The path for this operation is /user/{userId}. In order to make a usable path,
        // then we must make sure that {userId} is percent-escaped when it is substituted
        // into the path. So we intentionally introduce a path with spaces.
        let userRequestBuilder = UserAPI.getUserByNameWithRequestBuilder(username: "User Name With Spaces")
        let urlContainsSpace = userRequestBuilder.URLString.contains(" ")

        XCTAssert(!urlContainsSpace, "Expected URL to be escaped, but it was not.")
    }

}
