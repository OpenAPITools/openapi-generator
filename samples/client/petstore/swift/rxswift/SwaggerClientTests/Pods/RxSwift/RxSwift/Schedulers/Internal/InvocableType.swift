//
//  InvocableType.swift
//  Rx
//
//  Created by Krunoslav Zaher on 11/7/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

protocol InvocableType {
    func invoke()
}

protocol InvocableWithValueType {
    associatedtype Value

    func invoke(value: Value)
}