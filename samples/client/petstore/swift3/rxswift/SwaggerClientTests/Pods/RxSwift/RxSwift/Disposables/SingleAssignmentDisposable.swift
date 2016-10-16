//
//  SingleAssignmentDisposable.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/15/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents a disposable resource which only allows a single assignment of its underlying disposable resource.

If an underlying disposable resource has already been set, future attempts to set the underlying disposable resource will throw an exception.
*/
public class SingleAssignmentDisposable : DisposeBase, Disposable, Cancelable {
    private var _lock = SpinLock()
    
    // state
    private var _isDisposed = false
    private var _disposableSet = false
    private var _disposable = nil as Disposable?

    /**
    - returns: A value that indicates whether the object is disposed.
    */
    public var isDisposed: Bool {
        return _isDisposed
    }

    /**
    Initializes a new instance of the `SingleAssignmentDisposable`.
    */
    public override init() {
        super.init()
    }

    /**
    Gets or sets the underlying disposable. After disposal, the result of getting this property is undefined.
    
    **Throws exception if the `SingleAssignmentDisposable` has already been assigned to.**
    */
    public var disposable: Disposable {
        get {
            _lock.lock(); defer { _lock.unlock() }
            return _disposable ?? Disposables.create()
        }
        set {
            _setDisposable(newValue)?.dispose()
        }
    }

    private func _setDisposable(_ newValue: Disposable) -> Disposable? {
        _lock.lock(); defer { _lock.unlock() }
        if _disposableSet {
            rxFatalError("oldState.disposable != nil")
        }

        _disposableSet = true

        if _isDisposed {
            return newValue
        }

        _disposable = newValue

        return nil
    }

    /**
    Disposes the underlying disposable.
    */
    public func dispose() {
        if _isDisposed {
            return
        }
        _dispose()?.dispose()
    }

    private func _dispose() -> Disposable? {
        _lock.lock(); defer { _lock.unlock() }

        _isDisposed = true
        let disposable = _disposable
        _disposable = nil

        return disposable
    }
}
