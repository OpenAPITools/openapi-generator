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
        let transport =  URLSessionOpenAPITransport(baseURL: baseURL)
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

    func testGetPetById() {
        // Given
        let transport =  URLSessionOpenAPITransport(baseURL: baseURL)
        let api = PetAPI(transport)
        let petId: Int64 = 101
        let pet = Pet(id: petId, category: nil, name: "PetName101", photoUrls: [], tags: nil, status: .available)

        // When
        let expectation = expectation(description: "testGetPetByIdExpectation")
        api.addPet(pet: pet)
            // Delay to prevent too early access data
            .delay(for: 3.0, scheduler: DispatchQueue.global())
            .flatMap { _ in
                api.getPetById(petId: petId)
            }
            .sink { completion in
                switch completion {
                case .finished:
                    expectation.fulfill()
                case let .failure(error):
                    XCTFail("Finding pet operation finished with error: \(error)")
                    expectation.fulfill()
                }
            } receiveValue: { pet in
                XCTAssertTrue(pet.id == petId, "Found pet should have given pet id")
            }
            .store(in: &cancellable)

        wait(for: [expectation], timeout: timeout)
    }

    func testDeletePet() {
        // Given
        let transport =  URLSessionOpenAPITransport(baseURL: baseURL)
        let api = PetAPI(transport)
        let petId: Int64 = 102
        let pet = Pet(id: petId, category: nil, name: "PetName102", photoUrls: [], tags: nil, status: .available)

        // When
        let expectation = expectation(description: "testDeletePetExpectation")
        api.addPet(pet: pet)
            // Delay to prevent too early access data
            .delay(for: 3.0, scheduler: DispatchQueue.global())
            .flatMap { _ in
                api.deletePet(petId: petId, apiKey: "special-key")
            }
            .sink { completion in
                // Then
                switch completion {
                case .finished:
                    expectation.fulfill()
                case let .failure(error):
                    XCTFail("Deleting pet operation finished with error: \(error)")
                    expectation.fulfill()
                }
            } receiveValue: {}
            .store(in: &cancellable)
        wait(for: [expectation], timeout: timeout)
    }
}
