//
//  PetAPITests.swift
//  SwaggerClient
//
//  Created by Tony Wang on 7/31/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import XCTest
@testable import SwaggerClient

@available(OSX 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
class PetAPITests: XCTestCase {

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }

    func test1CreatePet() async throws {
        let category = Category(id: 1234, name: "eyeColor")
        let tags = [Tag(id: 1234, name: "New York"), Tag(id: 124321, name: "Jose")]
        let newPet = Pet(id: 1000, category: category, name: "Fluffy", photoUrls: ["https://petstore.com/sample/photo1.jpg", "https://petstore.com/sample/photo2.jpg"], tags: tags, status: .encodeValue(.available))

        try await PetAPI().addPet(body: newPet)
    }

    @MainActor func test2GetPet() {
        let expectation = self.expectation(description: "testGetPet")

        PetAPI().getPetById(petId: 1000) { (pet, error) in
            guard error == nil else {
                XCTFail("error retrieving pet")
                return
            }

            if let pet = pet {
                XCTAssert(pet.id == 1000, "invalid id")
                XCTAssert(pet.name == "Fluffy", "invalid name")

                expectation.fulfill()
            }
        }

        self.waitForExpectations(timeout: 10.0, handler: nil)
    }

    func test3UploadFile() async throws {
        guard
            let image = UIImage(color: .red, size: CGSize(width: 10, height: 10)),
            let imageData = image.pngData()
        else {
            fatalError()
        }

        do {
            let _ = try await PetAPI().uploadFile(petId: 1000, additionalMetadata: "additionalMetadata", file: imageData)
        } catch {
            XCTFail("error uploading file")
        }
    }

    func test4DeletePet() async throws {
        do {
            try await PetAPI().deletePet(petId: 1000)
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
                XCTFail("error deleting pet")
            }
        }
    }
}
