//
//  BinaryDisposable.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/12/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents two disposable resources that are disposed together.
*/
public final class BinaryDisposable : DisposeBase, Cancelable {

    private var _disposed: AtomicInt = 0

    // state
    private var _disposable1: Disposable?
    private var _disposable2: Disposable?

    /**
    - returns: Was resource disposed.
    */
    public var disposed: Bool {
        return _disposed > 0
    }

    /**
    Constructs new binary disposable from two disposables.

    - parameter disposable1: First disposable
    - parameter disposable2: Second disposable
    */
    init(_ disposable1: Disposable, _ disposable2: Disposable) {
        _disposable1 = disposable1
        _disposable2 = disposable2
        super.init()
    }

    /**
    Calls the disposal action if and only if the current instance hasn't been disposed yet.

    After invoking disposal action, disposal action will be dereferenced.
    */
    public func dispose() {
        if AtomicCompareAndSwap(0, 1, &_disposed) {
            _disposable1?.dispose()
            _disposable2?.dispose()
            _disposable1 = nil
            _disposable2 = nil
        }
    }
}
