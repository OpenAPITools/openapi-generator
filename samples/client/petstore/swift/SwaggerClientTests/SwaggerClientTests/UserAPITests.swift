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
    
    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    func testLogin() {
        let expectation = self.expectationWithDescription("testLogin")
        PetstoreClientAPI.UserAPI.loginUser(username: "swiftTester", password: "swift").execute().then { response -> Void in
                expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
                // The server isn't returning JSON - and currently the alamofire implementation
                // always parses responses as JSON, so making an exception for this here
                // Error Domain=NSCocoaErrorDomain Code=3840 "Invalid value around character 0." 
                // UserInfo={NSDebugDescription=Invalid value around character 0.}
                let error = errorType as NSError
                if error.code == 3840 {
                    expectation.fulfill()
                } else {
                    XCTFail("error logging in")
                }
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func testLogout() {
        let expectation = self.expectationWithDescription("testLogout")
        PetstoreClientAPI.UserAPI.logoutUser().execute().then { response -> Void in
                expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
                // The server gives us no data back so alamofire parsing fails - at least
                // verify that is the error we get here
                // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
                // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero 
                // length.}
                let error = errorType as NSError
                if error.code == -6006 {
                    expectation.fulfill()
                } else {
                    XCTFail("error logging out")
                }
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
}
