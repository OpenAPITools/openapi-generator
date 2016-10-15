//
//  PetAPITests.swift
//  SwaggerClient
//
//  Created by Tony Wang on 7/31/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import RxSwift
import XCTest
@testable import SwaggerClient

class PetAPITests: XCTestCase {

    let testTimeout = 10.0
    let disposeBag = DisposeBag()

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }

    func test1CreatePet() {
        let expectation = self.expectation(description: "testCreatePet")
        let newPet = Pet()
        let category = PetstoreClient.Category()
        category.id = 1234
        category.name = "eyeColor"
        newPet.category = category
        newPet.id = 1000
        newPet.name = "Fluffy"
        newPet.status = .available
        PetAPI.addPet(body: newPet).subscribe(onNext: {
            expectation.fulfill()
            }, onError: { errorType in
                XCTFail("error creating pet")
            }, onCompleted: nil, onDisposed: nil).addDisposableTo(disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetPet() {
        let expectation = self.expectation(description: "testGetPet")
        PetAPI.getPetById(petId: 1000).subscribe(onNext: { pet in
            XCTAssert(pet.id == 1000, "invalid id")
            XCTAssert(pet.name == "Fluffy", "invalid name")
            expectation.fulfill()
            }, onError: { errorType in
                XCTFail("error getting pet")
            }, onCompleted: nil, onDisposed: nil).addDisposableTo(disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test3DeletePet() {
        let expectation = self.expectation(description: "testDeletePet")
        PetAPI.deletePet(petId: 1000).subscribe(onNext: {
//            expectation.fulfill()
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
                    XCTFail("error deleting pet")
                }
            }, onCompleted: nil, onDisposed: nil).addDisposableTo(disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
}
