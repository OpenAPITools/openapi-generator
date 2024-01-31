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

        try await PetAPI.addPet(body: newPet)
    }

    func test2GetPet() async throws {
        let pet = try await PetAPI.getPetById(petId: 1000)
        
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
    }

    func test3UploadFile() async throws {
        let imageName = UUID().uuidString + ".png"

        guard
            let image = UIImage(color: .red, size: CGSize(width: 10, height: 10)),
            let imageURL = FileUtils.saveImage(imageName: imageName, image: image)
        else {
            fatalError()
        }

        do {
            let _ = try await PetAPI.uploadFile(petId: 1000, additionalMetadata: "additionalMetadata", file: imageURL)
            
            FileUtils.deleteFile(fileURL: imageURL)
        } catch {
            FileUtils.deleteFile(fileURL: imageURL)
            XCTFail("error uploading file")
        }
    }

    func test4DeletePet() async throws {
        do {
            try await PetAPI.deletePet(petId: 1000)
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
