//
//  StoreAPITests.swift
//  SwaggerClient
//
//  Created by Robin Eggenkamp on 5/21/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import XCTest
@testable import SwaggerClient

class StoreAPITests: XCTestCase {

    let isoDateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"

    let testTimeout = 10.0

    func test1PlaceOrder() {
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        let shipDate = Date()
        let order = PetstoreClientAPI.Order(id: 1000, petId: 1000, quantity: 10, shipDate: shipDate, status: .placed, complete: true)
        let expectation = self.expectation(description: "testPlaceOrder")

        PetstoreClientAPI.StoreAPI.placeOrder(body: order) { (order, error) in
            guard error == nil else {
                XCTFail("error placing order: \(error.debugDescription)")
                return
            }

            if let order = order {
                XCTAssert(order.id == 1000, "invalid id")
                XCTAssert(order.quantity == 10, "invalid quantity")
                XCTAssert(order.status == .placed, "invalid status")
                XCTAssert(order.shipDate!.isEqual(shipDate, format: self.isoDateFormat),
                          "Date should be idempotent")

                expectation.fulfill()
            }
        }

        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetOrder() {
        let expectation = self.expectation(description: "testGetOrder")

        PetstoreClientAPI.StoreAPI.getOrderById(orderId: 1000) { (order, error) in
            guard error == nil else {
                XCTFail("error retrieving order: \(error.debugDescription)")
                return
            }

            if let order = order {
                XCTAssert(order.id == 1000, "invalid id")
                XCTAssert(order.quantity == 10, "invalid quantity")
                XCTAssert(order.status == .placed, "invalid status")

                expectation.fulfill()
            }
        }
        
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test3DeleteOrder() {
        let expectation = self.expectation(description: "testDeleteOrder")

        PetstoreClientAPI.StoreAPI.deleteOrder(orderId: "1000") { (response, error) in
            guard error == nil else {
                XCTFail("error deleting order")
                return
            }

            guard let _ = response else {
                XCTFail("response is nil")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func testDownloadProgress() {
        let responseExpectation = self.expectation(description: "obtain response")
        let progressExpectation = self.expectation(description: "obtain progress")
        let requestBuilder = PetstoreClientAPI.StoreAPI.getOrderByIdWithRequestBuilder(orderId: 1000)

        requestBuilder.onProgressReady = { (_) in
            progressExpectation.fulfill()
        }

        requestBuilder.execute { _ in
            responseExpectation.fulfill()
        }

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
