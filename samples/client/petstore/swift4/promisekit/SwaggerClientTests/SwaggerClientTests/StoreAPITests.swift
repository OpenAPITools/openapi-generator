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
        let order = Order()
        let shipDate = Date()
        order.id = 1000
        order.petId = 1000
        order.complete = false
        order.quantity = 10
        order.shipDate = shipDate
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        order.status = Order.Status.placed
        let expectation = self.expectation(description: "testPlaceOrder")
        StoreAPI.placeOrder(body: order).then { order -> Void in
                XCTAssert(order.id == 1000, "invalid id")
                XCTAssert(order.quantity == 10, "invalid quantity")
                XCTAssert(order.status == .placed, "invalid status")
                XCTAssert(order.shipDate!.isEqual(shipDate, format: self.isoDateFormat),
                          "Date should be idempotent")

                expectation.fulfill()
            }.always {
                // Noop for now
            }.catch { errorType in
                XCTFail("error placing order")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test2GetOrder() {
        let expectation = self.expectation(description: "testGetOrder")
        StoreAPI.getOrderById(orderId: 1000).then { order -> Void in
            XCTAssert(order.id == 1000, "invalid id")
            XCTAssert(order.quantity == 10, "invalid quantity")
            XCTAssert(order.status == .placed, "invalid status")
            expectation.fulfill()
            }.always {
                // Noop for now
            }.catch { errorType in
                XCTFail("error placing order")
        }
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }
    
    func test3DeleteOrder() {
        let expectation = self.expectation(description: "testDeleteOrder")
        StoreAPI.deleteOrder(orderId: "1000").then {
            expectation.fulfill()
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
