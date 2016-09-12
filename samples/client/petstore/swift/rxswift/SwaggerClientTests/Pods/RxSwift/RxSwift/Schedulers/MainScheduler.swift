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

    private let _mainQueue: dispatch_queue_t

    var numberEnqueued: AtomicInt = 0

    private init() {
        _mainQueue = dispatch_get_main_queue()
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
    public static let asyncInstance = SerialDispatchQueueScheduler(serialQueue: dispatch_get_main_queue())

    /**
    In case this method is called on a background thread it will throw an exception.
    */
    public class func ensureExecutingOnScheduler(errorMessage: String? = nil) {
        if !NSThread.currentThread().isMainThread {
            rxFatalError(errorMessage ?? "Executing on backgound thread. Please use `MainScheduler.instance.schedule` to schedule work on main thread.")
        }
    }

    override func scheduleInternal<StateType>(state: StateType, action: StateType -> Disposable) -> Disposable {
        let currentNumberEnqueued = AtomicIncrement(&numberEnqueued)

        if NSThread.currentThread().isMainThread && currentNumberEnqueued == 1 {
            let disposable = action(state)
            AtomicDecrement(&numberEnqueued)
            return disposable
        }

        let cancel = SingleAssignmentDisposable()

        dispatch_async(_mainQueue) {
            if !cancel.disposed {
                action(state)
            }

            AtomicDecrement(&self.numberEnqueued)
        }

        return cancel
    }
}
