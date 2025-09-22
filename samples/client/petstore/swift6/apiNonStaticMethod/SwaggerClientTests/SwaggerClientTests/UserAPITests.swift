//
//  UserAPITests.swift
//  SwaggerClient
//
//  Created by Tony Wang on 7/31/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import PromiseKit
import RxSwift
import XCTest
@testable import SwaggerClient

@MainActor
@available(OSX 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
class UserAPITests: XCTestCase {

    let disposeBag = DisposeBag()

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
        UserAPI().loginUser(username: "swiftTester", password: "swift").subscribe(onNext: { _ in
            expectation.fulfill()
            }, onError: { errorType in
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
            }).disposed(by: disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func testLogout() {
        let expectation = self.expectation(description: "testLogout")
        UserAPI().logoutUser().done {
            expectation.fulfill()
        }.catch { _ in
                XCTFail("")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test1CreateUser() {
        let expectation = self.expectation(description: "testCreateUser")
        let newUser = User(id: 1000, username: "test@test.com", firstName: "Test", lastName: "Tester", email: "test@test.com", password: "test!", phone: "867-5309", userStatus: 0)
        UserAPI().createUser(body: newUser).subscribe(onNext: {
            expectation.fulfill()
            }, onError: { errorType in
                // The server gives us no data back so alamofire parsing fails - at least
                // verify that is the error we get here
                // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
                // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
                // length.}
                let error = errorType as NSError
                if error.code == -6006 {
                    expectation.fulfill()
                } else {
                    XCTFail("error creating user")
                }
            }).disposed(by: disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test2GetUser() {
        let expectation = self.expectation(description: "testGetUser")
        UserAPI().getUserByName(username: "test@test.com") { result in
            switch result {
            case let .success(user):
                XCTAssert(user.userStatus == 0, "invalid userStatus")
                XCTAssert(user.email == "test@test.com", "invalid email")
                XCTAssert(user.firstName == "Test", "invalid firstName")
                XCTAssert(user.lastName == "Tester", "invalid lastName")
                XCTAssert(user.password == "test!", "invalid password")
                XCTAssert(user.phone == "867-5309", "invalid phone")
                expectation.fulfill()

            case .failure:
                XCTFail("error getting user")
            }
        }
        
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test3DeleteUser() {
        let expectation = self.expectation(description: "testDeleteUser")
        UserAPI().deleteUser(username: "test@test.com").subscribe(onNext: {
            expectation.fulfill()
            }, onError: { errorType -> Void in
                // The server gives us no data back so alamofire parsing fails - at least
                // verify that is the error we get here
                // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
                // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
                // length.}
                let error = errorType as NSError
                if error.code == -6006 {
                    expectation.fulfill()
                } else {
                    XCTFail("error deleting user")
                }
            }).disposed(by: disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
}

