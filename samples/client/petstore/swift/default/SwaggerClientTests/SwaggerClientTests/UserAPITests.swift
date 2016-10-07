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

    func testLogin() {
        let expectation = self.expectationWithDescription("testLogin")

        UserAPI.loginUser(username: "swiftTester", password: "swift") { (_, error) in
            guard error == nil else {
                XCTFail("error logging in")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

    func testLogout() {
        let expectation = self.expectationWithDescription("testLogout")

        UserAPI.logoutUser { (error) in
            guard error == nil else {
                XCTFail("error logging out")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

    func test1CreateUser() {
        let expectation = self.expectationWithDescription("testCreateUser")

        let newUser = User()
        newUser.email = "test@test.com"
        // TODO comment out the following as dateOfBirth has been removed
        // from petstore.json, we'll need to add back the test after switching
        // to petstore-with-fake-endpoints-models-for-testing.yaml
        ////newUser.dateOfBirth = ISOFullDate.from(string: "1999-12-31")
        newUser.firstName = "Test"
        newUser.lastName = "Tester"
        newUser.id = 1000
        newUser.password = "test!"
        newUser.phone = "867-5309"
        newUser.username = "test@test.com"
        newUser.userStatus = 0

        UserAPI.createUser(body: newUser) { (error) in
            guard error == nil else {
                XCTFail("error creating user")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

    func test2GetUser() {
        let expectation = self.expectationWithDescription("testGetUser")

        UserAPI.getUserByName(username: "test@test.com") { (user, error) in
            guard error == nil else {
                XCTFail("error getting user")
                return
            }

            if let user = user {
                XCTAssert(user.userStatus == 0, "invalid userStatus")
                XCTAssert(user.email == "test@test.com", "invalid email")
                XCTAssert(user.firstName == "Test", "invalid firstName")
                XCTAssert(user.lastName == "Tester", "invalid lastName")
                XCTAssert(user.password == "test!", "invalid password")
                XCTAssert(user.phone == "867-5309", "invalid phone")
                // TODO comment out the following as dateOfBirth has been removed
                // from petstore.json, we'll need to add back the test after switching
                // to petstore-with-fake-endpoints-models-for-testing.yaml
                //XCTAssert(user.dateOfBirth?.description == "1999-12-31", "invalid date of birth")

                expectation.fulfill()
            }
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

    func test3DeleteUser() {
        let expectation = self.expectationWithDescription("testDeleteUser")

        UserAPI.deleteUser(username: "test@test.com") { (error) in
            guard error == nil else {
                XCTFail("error deleting user")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

}
