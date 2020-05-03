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

    let isoDateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZZZZZ"

    let testTimeout = 10.0

    func test1PlaceOrder() {
        let expectation = self.expectationWithDescription("testPlaceOrder")
        let shipDate = NSDate()

        let newOrder = Order()
        newOrder.id = 1000
        newOrder.petId = 1000
        newOrder.complete = false
        newOrder.quantity = 10
        newOrder.shipDate = shipDate
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        newOrder.status = Order.Status.Placed

        StoreAPI.placeOrder(body: newOrder) { (order, error) in
            guard error == nil else {
                XCTFail("error placing order: \(error.debugDescription)")
                return
            }

            if let order = order {
                XCTAssert(order.id == 1000, "invalid id")
                XCTAssert(order.quantity == 10, "invalid quantity")
                XCTAssert(order.status == .Placed, "invalid status")
                XCTAssert(order.shipDate!.isEqual(shipDate, format: self.isoDateFormat),
                          "Date should be idempotent")

                expectation.fulfill()
            }
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

    func test2GetOrder() {
        let expectation = self.expectationWithDescription("testGetOrder")

        StoreAPI.getOrderById(orderId: "1000") { (order, error) in
            guard error == nil else {
                XCTFail("error retrieving order: \(error.debugDescription)")
                return
            }

            if let order = order {
                XCTAssert(order.id == 1000, "invalid id")
                XCTAssert(order.quantity == 10, "invalid quantity")
                XCTAssert(order.status == .Placed, "invalid status")

                expectation.fulfill()
            }
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

    func test3DeleteOrder() {
        let expectation = self.expectationWithDescription("testDeleteOrder")

        StoreAPI.deleteOrder(orderId: "1000") { (error) in
            guard error == nil else {
                XCTFail("error deleting order")
                return
            }

            expectation.fulfill()
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

    func testDownloadProgress() {
        let responseExpectation = self.expectationWithDescription("obtain response")
        let progressExpectation = self.expectationWithDescription("obtain progress")
        let requestBuilder = StoreAPI.getOrderByIdWithRequestBuilder(orderId: "1000")

        requestBuilder.onProgressReady = { (progress) in
            progressExpectation.fulfill()
        }

        requestBuilder.execute { (_, _) in
            responseExpectation.fulfill()
        }

        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

}

private extension NSDate {

    /**
     Returns true if the dates are equal given the format string.

     - parameter date:   The date to compare to.
     - parameter format: The format string to use to compare.

     - returns: true if the dates are equal, given the format string.
     */
    func isEqual(date: NSDate, format: String) -> Bool {
        let fmt = NSDateFormatter()
        fmt.dateFormat = format
        return fmt.stringFromDate(self).isEqual(fmt.stringFromDate(date))
    }

}
