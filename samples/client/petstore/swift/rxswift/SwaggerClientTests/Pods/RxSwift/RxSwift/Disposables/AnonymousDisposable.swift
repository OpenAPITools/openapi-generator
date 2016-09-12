//
//  AnonymousDisposable.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/15/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents an Action-based disposable.

When dispose method is called, disposal action will be dereferenced.
*/
public final class AnonymousDisposable : DisposeBase, Cancelable {
    public typealias DisposeAction = () -> Void

    private var _disposed: AtomicInt = 0
    private var _disposeAction: DisposeAction?

    /**
    - returns: Was resource disposed.
    */
    public var disposed: Bool {
        return _disposed == 1
    }

    /**
    Constructs a new disposable with the given action used for disposal.

    - parameter disposeAction: Disposal action which will be run upon calling `dispose`.
    */
    public init(_ disposeAction: DisposeAction) {
        _disposeAction = disposeAction
        super.init()
    }

    /**
    Calls the disposal action if and only if the current instance hasn't been disposed yet.

    After invoking disposal action, disposal action will be dereferenced.
    */
    public func dispose() {
        if AtomicCompareAndSwap(0, 1, &_disposed) {
            assert(_disposed == 1)

            if let action = _disposeAction {
                _disposeAction = nil
                action()
            }
        }
    }
}
