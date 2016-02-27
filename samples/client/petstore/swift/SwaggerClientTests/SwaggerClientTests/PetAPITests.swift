//
//  PetAPITests.swift
//  SwaggerClient
//
//  Created by Joseph Zuromski on 2/8/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import PromiseKit
import XCTest
@testable import SwaggerClient

class PetAPITests: XCTestCase {
    
    let testTimeout = 10.0

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    func test1CreatePet() {
        let expectation = self.expectationWithDescription("testCreatePet")
        let newPet = Pet()
        let category = PetstoreClient.Category()
        category.id = 1234
        category.name = "eyeColor"
        newPet.category = category
        newPet.id = 1000
        newPet.name = "Fluffy"
        newPet.status = .Available
        PetAPI.addPet(body: newPet).then {
                expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
                XCTFail("error creating pet")
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func test2GetPet() {
        let expectation = self.expectationWithDescription("testGetPet")
        PetAPI.getPetById(petId: 1000).then { pet -> Void in
                XCTAssert(pet.id == 1000, "invalid id")
                XCTAssert(pet.name == "Fluffy", "invalid name")
                expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
                XCTFail("error creating pet")
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func test3DeletePet() {
        let expectation = self.expectationWithDescription("testDeletePet")
        PetAPI.deletePet(petId: 1000).always {
                // expectation.fulfill()
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
