//
//  UserAPITests.swift
//  SwaggerClient
//
//  Created by Tony Wang on 7/31/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import Combine
import XCTest
@testable import SwaggerClient

@available(OSX 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
class UserAPITests: XCTestCase {

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }

    func testLogin() async throws {
        do {
            let _ = try await UserAPI.loginUser(username: "swiftTester", password: "swift")
        } catch let errorType {
            // The server gives us no data back so alamofire parsing fails - at least
            // verify that is the error we get here
            // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
            // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
            // length.}
            let error = errorType as NSError
            if error.code == -6006 {
                // Everything ok!
            } else {
                XCTFail("error deleting order")
            }
        }
    }

    func testLogout() async throws {
        do {
            try await UserAPI.logoutUser()
        } catch let errorType {
            // The server gives us no data back so alamofire parsing fails - at least
            // verify that is the error we get here
            // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
            // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
            // length.}
            let error = errorType as NSError
            if error.code == -6006 {
                // Everything ok!
            } else {
                XCTFail("error deleting order")
            }
        }        
    }

    func test1CreateUser() async throws {
        let newUser = User(id: 1000, username: "test@test.com", firstName: "Test", lastName: "Tester", email: "test@test.com", password: "test!", phone: "867-5309", userStatus: 0)
        
        do {
            try await UserAPI.createUser(body: newUser)
        } catch let errorType {
            // The server gives us no data back so alamofire parsing fails - at least
            // verify that is the error we get here
            // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
            // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
            // length.}
            let error = errorType as NSError
            if error.code == -6006 {
                // Everything ok!
            } else {
                XCTFail("error creating user")
            }
        }
    }

    func test2GetUser() async throws {
        let user = try await UserAPI.getUserByName(username: "test@test.com")
        XCTAssert(user.userStatus == 0, "invalid userStatus")
        XCTAssert(user.email == "test@test.com", "invalid email")
        XCTAssert(user.firstName == "Test", "invalid firstName")
        XCTAssert(user.lastName == "Tester", "invalid lastName")
        XCTAssert(user.password == "test!", "invalid password")
        XCTAssert(user.phone == "867-5309", "invalid phone")
    }

    func test3DeleteUser() async throws {
        do {
            try await UserAPI.deleteUser(username: "test@test.com")
        } catch let errorType {
            // The server gives us no data back so alamofire parsing fails - at least
            // verify that is the error we get here
            // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
            // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
            // length.}
            let error = errorType as NSError
            if error.code == -6006 {
                // Everything ok!
            } else {
                XCTFail("error deleting user")
            }
        }
    }

}
