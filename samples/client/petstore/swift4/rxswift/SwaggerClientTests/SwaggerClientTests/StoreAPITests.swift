//
//  StoreAPITests.swift
//  SwaggerClient
//
//  Created by Tony Wang on 7/31/16.
//  Copyright © 2016 Swagger. All rights reserved.
//

import PetstoreClient
import RxSwift
import XCTest
@testable import SwaggerClient

class StoreAPITests: XCTestCase {

    let isoDateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"

    let testTimeout = 10.0
    let disposeBag = DisposeBag()

    func test1PlaceOrder() {
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        let shipDate = Date()
        let order = Order(_id: 1000, petId: 1000, quantity: 10, shipDate: shipDate, status: .placed, complete: true)
        let expectation = self.expectation(description: "testPlaceOrder")
        StoreAPI.placeOrder(body: order).subscribe(onNext: { order in
            XCTAssert(order._id == 1000, "invalid id")
            XCTAssert(order.quantity == 10, "invalid quantity")
            XCTAssert(order.status == .placed, "invalid status")
            XCTAssert(order.shipDate!.isEqual(shipDate, format: self.isoDateFormat),
                "Date should be idempotent")
            XCTAssert(order.complete == true, "invalid complete")

            expectation.fulfill()
        }, onError: { errorType in
            XCTFail("error placing order")
        }).disposed(by: disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test2GetOrder() {
        let expectation = self.expectation(description: "testGetOrder")
        StoreAPI.getOrderById(orderId: 1000).subscribe(onNext: { order -> Void in
            XCTAssert(order._id == 1000, "invalid id")
            XCTAssert(order.quantity == 10, "invalid quantity")
            XCTAssert(order.status == .placed, "invalid status")
            XCTAssert(order.complete == true, "invalid complete")
            expectation.fulfill()
            }, onError: { errorType in
                XCTFail("error placing order")
        }).disposed(by: disposeBag)
        self.waitForExpectations(timeout: testTimeout, handler: nil)
    }

    func test3DeleteOrder() {
        let expectation = self.expectation(description: "testDeleteOrder")
        StoreAPI.deleteOrder(orderId: "1000").subscribe(onNext: {
            expectation.fulfill()
            }, onError: { errorType -> Void in
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
        }).disposed(by: disposeBag)
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
