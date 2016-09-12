//
//  DisposeBag.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/25/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

extension Disposable {
    /**
    Adds `self` to `bag`.

    - parameter bag: `DisposeBag` to add `self` to.
    */
    public func addDisposableTo(bag: DisposeBag) {
        bag.addDisposable(self)
    }
}

/**
Thread safe bag that disposes added disposables on `deinit`.

This returns ARC (RAII) like resource management to `RxSwift`.

In case contained disposables need to be disposed, just put a different dispose bag
or create a new one in its place.

    self.existingDisposeBag = DisposeBag()

In case explicit disposal is necessary, there is also `CompositeDisposable`.
*/
public class DisposeBag: DisposeBase {

    private var _lock = SpinLock()

    // state
    private var _disposables = [Disposable]()
    private var _disposed = false

    /**
    Constructs new empty dispose bag.
    */
    public override init() {
        super.init()
    }

    /**
    Adds `disposable` to be disposed when dispose bag is being deinited.

    - parameter disposable: Disposable to add.
    */
    public func addDisposable(disposable: Disposable) {
        _addDisposable(disposable)?.dispose()
    }

    private func _addDisposable(disposable: Disposable) -> Disposable? {
        _lock.lock(); defer { _lock.unlock() }
        if _disposed {
            return disposable
        }

        _disposables.append(disposable)

        return nil
    }

    /**
    This is internal on purpose, take a look at `CompositeDisposable` instead.
    */
    private func dispose() {
        let oldDisposables = _dispose()

        for disposable in oldDisposables {
            disposable.dispose()
        }
    }

    private func _dispose() -> [Disposable] {
        _lock.lock(); defer { _lock.unlock() }

        let disposables = _disposables

        _disposables.removeAll(keepCapacity: false)
        _disposed = true

        return disposables
    }

    deinit {
        dispose()
    }
}