//
//  PetAPITests.swift
//  SwaggerClient
//
//  Created by Tony Wang on 7/31/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import XCTest
import Combine
@testable import SwaggerClient

@available(OSX 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
class PetAPITests: XCTestCase {

    let testTimeout = 10.0

    var anyCancellables: [AnyCancellable] = []

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

        PetAPI.addPet(body: newPet).sink(receiveCompletion: { (completion) in
            switch completion {
            case .failure:
                XCTFail("error creating pet")
            case .finished: ()
            }
        }, receiveValue: { _ in
            expectation.fulfill()
        }).store(in: &anyCancellables)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetPet() {
        let expectation = self.expectation(description: "testGetPet")
        PetAPI.getPetById(petId: 1000).sink(receiveCompletion: { (completion) in
            switch completion {
            case .failure:
                XCTFail("error getting pet")
            case .finished:()
            }
        }, receiveValue: { pet in
            XCTAssert(pet.id == 1000, "invalid id")
            XCTAssert(pet.name == "Fluffy", "invalid name")
            XCTAssert(pet.category!.id == 1234, "invalid category id")
            XCTAssert(pet.category!.name == "eyeColor", "invalid category name")

            let tag1 = pet.tags![0]
            XCTAssert(tag1.id == 1234, "invalid tag id")
            XCTAssert(tag1.name == "New York", "invalid tag name")

            let tag2 = pet.tags![1]
            XCTAssert(tag2.id == 124321, "invalid tag id")
            XCTAssert(tag2.name == "Jose", "invalid tag name")

            XCTAssert(pet.photoUrls[0] == "https://petstore.com/sample/photo1.jpg")
            XCTAssert(pet.photoUrls[1] == "https://petstore.com/sample/photo2.jpg")

            expectation.fulfill()
        }).store(in: &anyCancellables)
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

        PetAPI.uploadFile(petId: 1000, additionalMetadata: "additionalMetadata", file: imageURL).sink(receiveCompletion: { (completion) in
            switch completion {
            case .failure:
                FileUtils.deleteFile(fileURL: imageURL)
                XCTFail("error uploading file")
            case .finished:()
            }
        }, receiveValue: { _ in
            FileUtils.deleteFile(fileURL: imageURL)

            expectation.fulfill()
        }).store(in: &anyCancellables)

        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test4DeletePet() {
        let expectation = self.expectation(description: "testDeletePet")
        PetAPI.deletePet(petId: 1000).sink(receiveCompletion: { (completion) in
            switch completion {
            case .failure(let errorType):
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
            case .finished:()
            }
        }, receiveValue: { _ in
            expectation.fulfill()
        }).store(in: &anyCancellables)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
}
