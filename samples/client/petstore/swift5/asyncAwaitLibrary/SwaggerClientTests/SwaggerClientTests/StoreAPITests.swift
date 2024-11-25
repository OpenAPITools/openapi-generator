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

    func test1PlaceOrder() async throws {
        // use explicit naming to reference the enum so that we test we don't regress on enum naming
        let shipDate = Date()
        let order = Order(id: 1000, petId: 1000, quantity: 10, shipDate: shipDate, status: .placed, complete: true)
        let placedOrder = try await StoreAPI.placeOrder(body: order)
                    
        XCTAssert(placedOrder.id == 1000, "invalid id")
        XCTAssert(placedOrder.quantity == 10, "invalid quantity")
        XCTAssert(placedOrder.status == .placed, "invalid status")
        XCTAssert(placedOrder.shipDate!.isEqual(shipDate, format: self.isoDateFormat),
                  "Date should be idempotent")
        XCTAssert(placedOrder.complete == true, "invalid complete")
    }

    func test2GetOrder() async throws {
        let order = try await StoreAPI.getOrderById(orderId: 1000)
        XCTAssert(order.id == 1000, "invalid id")
        XCTAssert(order.quantity == 10, "invalid quantity")
        XCTAssert(order.status == .placed, "invalid status")
        XCTAssert(order.complete == true, "invalid complete")
    }

    func test3DeleteOrder() async throws {
        do {
            try await StoreAPI.deleteOrder(orderId: "1000")
        } catch let errorType {
            // The server gives us no data back so alamofire parsing fails - at least
            // verify that is the error we get here
            // Error Domain=com.alamofire.error Code=-6006 "JSON could not be serialized. Input data was nil or zero
            // length." UserInfo={NSLocalizedFailureReason=JSON could not be serialized. Input data was nil or zero
            // length.}
            let error = errorType as NSError
            if error.code == -6006 {
                // Everything ok!
            } else {
                XCTFail("error deleting order")
            }
        }
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
