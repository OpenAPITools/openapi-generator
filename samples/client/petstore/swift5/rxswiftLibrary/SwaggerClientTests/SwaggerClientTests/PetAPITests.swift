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
        let category = Category(id: 1234, name: "eyeColor")
        let tags = [Tag(id: 1234, name: "New York"), Tag(id: 124321, name: "Jose")]
        let newPet = Pet(id: 1000, category: category, name: "Fluffy", photoUrls: ["https://petstore.com/sample/photo1.jpg", "https://petstore.com/sample/photo2.jpg"], tags: tags, status: .encodeValue(.available))

        PetAPI.addPet(body: newPet).subscribe(onNext: {
            expectation.fulfill()
            }, onError: { _ in
                XCTFail("error creating pet")
            }).disposed(by: disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetPet() {
        let expectation = self.expectation(description: "testGetPet")
        PetAPI.getPetById(petId: 1000).subscribe(onNext: { pet in
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
            }, onError: { _ in
                XCTFail("error getting pet")
            }).disposed(by: disposeBag)
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

        PetAPI.uploadFile(petId: 1000, additionalMetadata: "additionalMetadata", file: imageURL).subscribe(onNext: { _ in
            FileUtils.deleteFile(fileURL: imageURL)
            expectation.fulfill()
        }, onError: { _ in
            FileUtils.deleteFile(fileURL: imageURL)
            XCTFail("error uploading file")
        }).disposed(by: disposeBag)

        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test4DeletePet() {
        let expectation = self.expectation(description: "testDeletePet")
        PetAPI.deletePet(petId: 1000).subscribe(onNext: {
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
                    XCTFail("error deleting pet")
                }
            }).disposed(by: disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
}
