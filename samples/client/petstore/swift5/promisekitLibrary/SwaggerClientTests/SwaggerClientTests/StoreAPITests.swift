//
//  StoreAPITests.swift
//  SwaggerClient
//
//  Created by Joseph Zuromski on 2/8/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import PromiseKit
import XCTest
@testable import SwaggerClient

class StoreAPITests: XCTestCase {

    let isoDateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"

    let testTimeout = 10.0

    func test1PlaceOrder() {
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        let shipDate = Date()
        let order = Order(id: 1000, petId: 1000, quantity: 10, shipDate: shipDate, status: .placed, complete: true)
        let expectation = self.expectation(description: "testPlaceOrder")
        StoreAPI.placeOrder(body: order).done { order in
                XCTAssert(order.id == 1000, "invalid id")
                XCTAssert(order.quantity == 10, "invalid quantity")
                XCTAssert(order.status == .placed, "invalid status")
                XCTAssert(order.shipDate!.isEqual(shipDate, format: self.isoDateFormat),
                          "Date should be idempotent")

                expectation.fulfill()
            }.catch { _ in
                XCTFail("error placing order")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetOrder() {
        let expectation = self.expectation(description: "testGetOrder")
        StoreAPI.getOrderById(orderId: 1000).done { order in
            XCTAssert(order.id == 1000, "invalid id")
            XCTAssert(order.quantity == 10, "invalid quantity")
            XCTAssert(order.status == .placed, "invalid status")
            expectation.fulfill()
            }.catch { _ in
                XCTFail("error placing order")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test3DeleteOrder() {
        let expectation = self.expectation(description: "testDeleteOrder")
        StoreAPI.deleteOrder(orderId: "1000").done {
            expectation.fulfill()
            }.catch { (_) in
                XCTFail("error deleting order")
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
