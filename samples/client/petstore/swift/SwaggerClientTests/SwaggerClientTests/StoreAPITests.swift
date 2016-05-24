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
        let expectation = self.expectationWithDescription("testPlaceOrder")
        
        let newOrder = Order()
        newOrder.id = 1000
        newOrder.petId = 1000
        newOrder.complete = false
        newOrder.quantity = 10
        newOrder.shipDate = NSDate()
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        newOrder.status = Order.Status.Placed
        
        StoreAPI.placeOrder(body: newOrder) { (order, error) in
            guard error == nil else {
                XCTFail("error placing order")
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
    
    func test2GetOrder() {
        let expectation = self.expectationWithDescription("testGetOrder")
        
        StoreAPI.getOrderById(orderId: "1000") { (order, error) in
            guard error == nil else {
                XCTFail("error retrieving order")
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
            // The server gives us no data back so Alamofire parsing fails - at least
            // verify that is the error we get here
            // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
            // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
            // length.}
            guard let error = error where error._code == -6006 else {
                XCTFail("error deleting order")
                return
            }
            
            expectation.fulfill()
        }
        
        self.waitForExpectationsWithTimeout(testTimeout, handler: nil)
    }

}
