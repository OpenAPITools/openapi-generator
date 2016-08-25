//
//  Rx.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/14/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

#if TRACE_RESOURCES
/// Counts internal Rx resource allocations (Observables, Observers, Disposables, etc.). This provides a simple way to detect leaks during development.
public var resourceCount: AtomicInt = 0
#endif

/// Swift does not implement abstract methods. This method is used as a runtime check to ensure that methods which intended to be abstract (i.e., they should be implemented in subclasses) are not called directly on the superclass.
@noreturn func abstractMethod() -> Void {
    rxFatalError("Abstract method")
}

@noreturn func rxFatalError(lastMessage: String) {
    // The temptation to comment this line is great, but please don't, it's for your own good. The choice is yours.
    fatalError(lastMessage)
}

func incrementChecked(inout i: Int) throws -> Int {
    if i == Int.max {
        throw RxError.Overflow
    }
    let result = i
    i += 1
    return result
}

func decrementChecked(inout i: Int) throws -> Int {
    if i == Int.min {
        throw RxError.Overflow
    }
    let result = i
    i -= 1
    return result
}
