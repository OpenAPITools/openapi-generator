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
            }.catch { (error) in
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
            }.catch { (error) in
                XCTFail("")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test1CreateUser() {
        let expectation = self.expectation(description: "testCreateUser")
        let newUser = User(_id: 1000, username: "test@test.com", firstName: "Test", lastName: "Tester", email: "test@test.com", password: "test!", phone: "867-5309", userStatus: 0)
        UserAPI.createUser(body: newUser).then {
            expectation.fulfill()
            }.always {
                // Noop for now
            }.catch { (error) in
                XCTFail("create user error")
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
            }.always {
                // Noop for now
            }.catch { (error) in
                XCTFail("deleting user error")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

}
