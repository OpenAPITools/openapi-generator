//
//  PetAPITests.swift
//  
//
//  Created by Anton Davydov on 16.11.2021.
//

import XCTest
import Combine
import PetstoreOpenAPI
import OpenAPITransport

class PetAPITests: XCTestCase {
    var cancellable = Set<AnyCancellable>()
    let timeout: TimeInterval = 10
    let baseURL = URL(string: "https://petstore.swagger.io/v2")!

    override func tearDown() {
        cancellable.removeAll()
    }

    func testAddPet() {
        // Given
        let transport =  URLSessionOpenAPITransport(config: .init(baseURL: baseURL))
        
        let api = PetAPI(transport)
        let category = Category(id: 1, name: "CategoryName")
        let photoUrls = ["https://petstore.com/sample/photo1.jpg", "https://petstore.com/sample/photo2.jpg"]
        let tags = [Tag(id: 10, name: "Tag1"), Tag(id: 11, name: "Tag2")]
        let pet = Pet(
            id: 100,
            category: category,
            name: "PetName100",
            photoUrls: photoUrls,
            tags: tags,
            status: .available
        )

        // When
        let expectation = expectation(description: "addPetTestExpectation")
        api.addPet(pet: pet)
            .sink(receiveCompletion: { completion in
                // Then
                switch completion {
                case .finished:
                    expectation.fulfill()
                case let .failure(error):
                    XCTFail("Adding pet operation finished with error: \(error)")
                    expectation.fulfill()
                }
            }, receiveValue: { addedPet in
                // Then
                XCTAssertTrue(pet == addedPet, "Added pet should be the same as given value")
            })
            .store(in: &cancellable)
        wait(for: [expectation], timeout: timeout)
    }

    func testGetPetByUnknownId() {
        // Given
        let transport =  URLSessionOpenAPITransport(config: .init(baseURL: baseURL))
        let api = PetAPI(transport)
        let unknownPetId: Int64 = 1010101010

        // When
        let expectation = expectation(description: "testGetPetByIdExpectation")
        api.getPetById(petId: unknownPetId)
            .sink { completion in
                switch completion {
                case .finished:
                    XCTFail("Finding unknown pet operation should return 404 error")
                case let .failure(error):
                    XCTAssertTrue((error as? PetAPI.GetPetByIdError) == .code404Error, "Finding unknown pet operation should return 404 error")
                }
                expectation.fulfill()
            } receiveValue: { _ in }
            .store(in: &cancellable)

        wait(for: [expectation], timeout: timeout)
    }

    func testDeleteUnknownPet() {
        // Given
        let transport =  URLSessionOpenAPITransport(config: .init(baseURL: baseURL))
        let api = PetAPI(transport)
        let unknownPetId: Int64 = 1010101010

        // When
        let expectation = expectation(description: "testDeletePetExpectation")
        api
            .deletePet(petId: unknownPetId, apiKey: "special-key")
            .sink { completion in
                // Then
                switch completion {
                case .finished:
                    XCTFail("Deleting unknown pet operation should return 404 error")
                    expectation.fulfill()
                case let .failure(error):
                    XCTAssertTrue((error as? OpenAPITransportError)?.statusCode == 404, "Deleting unknown pet operation should return 404 error")
                    expectation.fulfill()
                }
            } receiveValue: {}
            .store(in: &cancellable)
        wait(for: [expectation], timeout: timeout)
    }
}

extension Tag: Equatable {
    public static func ==(l: Tag, r: Tag) -> Bool {
        l.id == r.id && l.name == r.name
    }
}

extension Pet: Equatable {
    public static func ==(l: Pet, r: Pet) -> Bool {
        l.id == r.id && l.name == r.name && l.photoUrls == r.photoUrls && l.status == r.status && l.tags == r.tags
    }
}
