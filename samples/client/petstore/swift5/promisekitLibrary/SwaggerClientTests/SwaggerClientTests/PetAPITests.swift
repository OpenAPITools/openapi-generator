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
        let expectation = self.expectation(description: "testCreatePet")
        let category = PetstoreClient.Category(id: 1234, name: "eyeColor")
        let tags = [Tag(id: 1234, name: "New York"), Tag(id: 124321, name: "Jose")]
        let newPet = Pet(id: 1000, category: category, name: "Fluffy", photoUrls: ["https://petstore.com/sample/photo1.jpg", "https://petstore.com/sample/photo2.jpg"], tags: tags, status: .available)

        PetAPI.addPet(body: newPet).done {
                expectation.fulfill()
            }.catch { _ in
                XCTFail("error creating pet")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetPet() {
        let expectation = self.expectation(description: "testGetPet")
        PetAPI.getPetById(petId: 1000).done { pet in
                XCTAssert(pet.id == 1000, "invalid id")
                XCTAssert(pet.name == "Fluffy", "invalid name")
                expectation.fulfill()
            }.catch { _ in
                XCTFail("error creating pet")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test3UploadFile() {
        let expectation = self.expectation(description: "testUploadFile")

        let imageName = UUID().uuidString + ".png"

        guard
            let image = UIImage(color: .red, size: CGSize(width: 10, height: 10)),
            let imageURL = FileUtils.saveImage(imageName: imageName, image: image)
        else {
            fatalError()
        }

        PetAPI.uploadFile(petId: 1000, additionalMetadata: "additionalMetadata", file: imageURL).done { _ in
                FileUtils.deleteFile(fileURL: imageURL)
                expectation.fulfill()
            }.catch { _ in
                FileUtils.deleteFile(fileURL: imageURL)
                XCTFail("error uploading file")
        }

        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test4DeletePet() {
        let expectation = self.expectation(description: "testDeletePet")
        PetAPI.deletePet(petId: 1000).done {
            expectation.fulfill()
            }.catch { (_) in
                XCTFail("error deleting pet")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
}
