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
    
    let testTimeout = 10.0

    override func setUp() {
        super.setUp()
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }
    
    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        super.tearDown()
    }
    
    func test1PlaceOrder() {
        let order = Order()
        order.id = 1000
        order.petId = 1000
        order.complete = false
        order.quantity = 10
        order.shipDate = NSDate()
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        order.status = Order.Status.Placed
        let expectation = self.expectationWithDescription("testPlaceOrder")
        StoreAPI.placeOrder(body: order).then { order -> Void in
                XCTAssert(order.id == 1000, "invalid id")
                XCTAssert(order.quantity == 10, "invalid quantity")
                XCTAssert(order.status == .Placed, "invalid status")
                expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
                XCTFail("error placing order")
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func test2GetOrder() {
        let expectation = self.expectationWithDescription("testGetOrder")
        StoreAPI.getOrderById(orderId: "1000").then { order -> Void in
            XCTAssert(order.id == 1000, "invalid id")
            XCTAssert(order.quantity == 10, "invalid quantity")
            XCTAssert(order.status == .Placed, "invalid status")
            expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
                XCTFail("error placing order")
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }
    
    func test3DeleteOrder() {
        let expectation = self.expectationWithDescription("testDeleteOrder")
        StoreAPI.deleteOrder(orderId: "1000").then {
                expectation.fulfill()
            }.always {
                // Noop for now
            }.error { errorType -> Void in
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
        }
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

}
