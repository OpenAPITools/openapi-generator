//
//  StoreAPITests.swift
//  SwaggerClient
//
//  Created by Tony Wang on 7/31/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import Combine
import XCTest
@testable import SwaggerClient

@available(OSX 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
class StoreAPITests: XCTestCase {

    let isoDateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"

    let testTimeout = 10.0

    var anyCancellables: [AnyCancellable] = []

    func test1PlaceOrder() {
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        let shipDate = Date()
        let order = Order(id: 1000, petId: 1000, quantity: 10, shipDate: shipDate, status: .placed, complete: true)
        let expectation = self.expectation(description: "testPlaceOrder")
        StoreAPI.placeOrder(body: order).sink(receiveCompletion: { (completion) in
            switch completion {
            case .failure:
                XCTFail("error placing order")
            case .finished:()
            }
        }, receiveValue: { order in
            XCTAssert(order.id == 1000, "invalid id")
            XCTAssert(order.quantity == 10, "invalid quantity")
            XCTAssert(order.status == .placed, "invalid status")
            XCTAssert(order.shipDate!.isEqual(shipDate, format: self.isoDateFormat),
                      "Date should be idempotent")
            XCTAssert(order.complete == true, "invalid complete")

            expectation.fulfill()
        }).store(in: &anyCancellables)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetOrder() {
        let expectation = self.expectation(description: "testGetOrder")
        StoreAPI.getOrderById(orderId: 1000).sink(receiveCompletion: { (completion) in
            switch completion {
            case .failure:
                XCTFail("error placing order")
            case .finished:()
            }
        }, receiveValue: { order in
            XCTAssert(order.id == 1000, "invalid id")
            XCTAssert(order.quantity == 10, "invalid quantity")
            XCTAssert(order.status == .placed, "invalid status")
            XCTAssert(order.complete == true, "invalid complete")
            expectation.fulfill()
        }).store(in: &anyCancellables)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test3DeleteOrder() {
        let expectation = self.expectation(description: "testDeleteOrder")
        StoreAPI.deleteOrder(orderId: "1000").sink(receiveCompletion: { (completion) in
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
                    XCTFail("error deleting order")
                }
            case .finished:()
            }
        }, receiveValue: { _ in
            expectation.fulfill()
        }).store(in: &anyCancellables)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

}

private extension Date {

    /**
     Returns true if the dates are equal given the format string.
     
     - parameter date:   The date to compare to.
     - parameter format: The format string to use to compare.
     
     - returns: true if the dates are equal, given the format string.
     */
    func isEqual(_ date: Date, format: String) -> Bool {
        let fmt = DateFormatter()
        fmt.dateFormat = format
        return fmt.string(from: self).isEqual(fmt.string(from: date))
    }

}
