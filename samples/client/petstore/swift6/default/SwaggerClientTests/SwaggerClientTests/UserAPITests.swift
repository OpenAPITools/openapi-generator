//
//  UserAPITests.swift
//  SwaggerClient
//
//  Created by Robin Eggenkamp on 5/21/16.
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
}
