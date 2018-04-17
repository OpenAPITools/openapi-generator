//
//  PetAPITests.swift
//  SwaggerClient
//
//  Created by Robin Eggenkamp on 5/21/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
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
        let expectation = self.expectation(description: "testCreatePet")
        let category = PetstoreClient.Category(_id: 1234, name: "eyeColor")
        let tags = [Tag(_id: 1234, name: "New York"), Tag(_id: 124321, name: "Jose")]
        let newPet = Pet(_id: 1000, category: category, name: "Fluffy", photoUrls: ["https://petstore.com/sample/photo1.jpg", "https://petstore.com/sample/photo2.jpg"], tags: tags, status: .available)

        PetAPI.addPet(body: newPet) { (response, error) in
            guard error == nil else {
                XCTFail("error creating pet")
                return
            }
            
            expectation.fulfill()
        }
        
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test2GetPet() {
        let expectation = self.expectation(description: "testGetPet")
        
        PetAPI.getPetById(petId: 1000) { (pet, error) in
            guard error == nil else {
                XCTFail("error retrieving pet")
                return
            }
            
            if let pet = pet {
                XCTAssert(pet._id == 1000, "invalid id")
                XCTAssert(pet.name == "Fluffy", "invalid name")
                
                expectation.fulfill()
            }
        }
        
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test3DeletePet() {
        let expectation = self.expectation(description: "testDeletePet")
        
        PetAPI.deletePet(petId: 1000) { (response, error) in
            guard error == nil else {
                XCTFail("error deleting pet")
                return
            }

            expectation.fulfill()
        }
        
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

}
