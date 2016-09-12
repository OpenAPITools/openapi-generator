//
//  ObserverType.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/8/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Supports push-style iteration over an observable sequence.
*/
public protocol ObserverType {
    /**
    The type of elements in sequence that observer can observe.
    */
    associatedtype E

    /**
    Notify observer about sequence event.

    - parameter event: Event that occurred.
    */
    func on(event: Event<E>)
}

/**
Convenience API extensions to provide alternate next, error, completed events
*/
public extension ObserverType {

    /**
    Convenience method equivalent to `on(.Next(element: E))`

    - parameter element: Next element to send to observer(s)
    */
    final func onNext(element: E) {
        on(.Next(element))
    }

    /**
    Convenience method equivalent to `on(.Completed)`
    */
    final func onCompleted() {
        on(.Completed)
    }

    /**
    Convenience method equivalent to `on(.Error(error: ErrorType))`
    - parameter error: ErrorType to send to observer(s)
    */
    final func onError(error: ErrorType) {
        on(.Error(error))
    }
}
