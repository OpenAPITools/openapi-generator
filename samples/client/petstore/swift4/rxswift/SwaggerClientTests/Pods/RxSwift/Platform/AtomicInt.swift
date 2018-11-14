//
//  AtomicInt.swift
//  Platform
//
//  Created by Krunoslav Zaher on 10/28/18.
//  Copyright © 2018 Krunoslav Zaher. All rights reserved.
//

import RxAtomic

typealias AtomicInt = RxAtomic.AtomicInt

extension AtomicInt {
    init(_ initialValue: Int32) {
        self.init()
        self.initialize(initialValue)
    }

    @discardableResult
    mutating func increment() -> Int32 {
        return self.add(1)
    }

    @discardableResult
    mutating func decrement() -> Int32 {
        return self.sub(1)
    }

    mutating func isFlagSet(_ mask: Int32) -> Bool {
        return (self.load() & mask) != 0
    }
}

