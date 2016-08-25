//
//  BooleanDisposable.swift
//  Rx
//
//  Created by Junior B. on 10/29/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents a disposable resource that can be checked for disposal status.
*/
public class BooleanDisposable : Disposable, Cancelable {

    internal static let BooleanDisposableTrue = BooleanDisposable(disposed: true)
    private var _disposed = false

    /**
        Initializes a new instance of the `BooleanDisposable` class
     */
    public init() {
    }

    /**
        Initializes a new instance of the `BooleanDisposable` class with given value
     */
    public init(disposed: Bool) {
        self._disposed = disposed
    }

    /**
        - returns: Was resource disposed.
     */
    public var disposed: Bool {
        return _disposed
    }

    /**
        Sets the status to disposed, which can be observer through the `disposed` property.
     */
    public func dispose() {
        _disposed = true
    }
}