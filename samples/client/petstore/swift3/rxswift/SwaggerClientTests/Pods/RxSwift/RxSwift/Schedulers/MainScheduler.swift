//
//  MainScheduler.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/8/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Abstracts work that needs to be performed on `MainThread`. In case `schedule` methods are called from main thread, it will perform action immediately without scheduling.

This scheduler is usually used to perform UI work.

Main scheduler is a specialization of `SerialDispatchQueueScheduler`.

This scheduler is optimized for `observeOn` operator. To ensure observable sequence is subscribed on main thread using `subscribeOn`
operator please use `ConcurrentMainScheduler` because it is more optimized for that purpose.
*/
public final class MainScheduler : SerialDispatchQueueScheduler {

    private let _mainQueue: DispatchQueue

    var numberEnqueued: AtomicInt = 0

    private init() {
        _mainQueue = DispatchQueue.main
        super.init(serialQueue: _mainQueue)
    }

    /**
    Singleton instance of `MainScheduler`
    */
    public static let instance = MainScheduler()

    /**
    Singleton instance of `MainScheduler` that always schedules work asynchronously
    and doesn't perform optimizations for calls scheduled from main thread.
    */
    public static let asyncInstance = SerialDispatchQueueScheduler(serialQueue: DispatchQueue.main)

    /**
    In case this method is called on a background thread it will throw an exception.
    */
    public class func ensureExecutingOnScheduler(errorMessage: String? = nil) {
        if !Thread.current.isMainThread {
            rxFatalError(errorMessage ?? "Executing on backgound thread. Please use `MainScheduler.instance.schedule` to schedule work on main thread.")
        }
    }

    override func scheduleInternal<StateType>(_ state: StateType, action: @escaping (StateType) -> Disposable) -> Disposable {
        let currentNumberEnqueued = AtomicIncrement(&numberEnqueued)

        if Thread.current.isMainThread && currentNumberEnqueued == 1 {
            let disposable = action(state)
            _ = AtomicDecrement(&numberEnqueued)
            return disposable
        }

        let cancel = SingleAssignmentDisposable()

        _mainQueue.async {
            if !cancel.isDisposed {
                _ = action(state)
            }

            _ = AtomicDecrement(&self.numberEnqueued)
        }

        return cancel
    }
}
