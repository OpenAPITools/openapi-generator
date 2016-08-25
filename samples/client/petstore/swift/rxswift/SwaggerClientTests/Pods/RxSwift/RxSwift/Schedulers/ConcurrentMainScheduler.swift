//
//  ConcurrentMainScheduler.swift
//  Rx
//
//  Created by Krunoslav Zaher on 10/17/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Abstracts work that needs to be performed on `MainThread`. In case `schedule` methods are called from main thread, it will perform action immediately without scheduling.

This scheduler is optimized for `subscribeOn` operator. If you want to observe observable sequence elements on main thread using `observeOn` operator,
`MainScheduler` is more suitable for that purpose.
*/
public final class ConcurrentMainScheduler : SchedulerType {
    public typealias TimeInterval = NSTimeInterval
    public typealias Time = NSDate

    private let _mainScheduler: MainScheduler
    private let _mainQueue: dispatch_queue_t

    /**
    - returns: Current time.
    */
    public var now : NSDate {
        return _mainScheduler.now
    }

    private init(mainScheduler: MainScheduler) {
        _mainQueue = dispatch_get_main_queue()
        _mainScheduler = mainScheduler
    }

    /**
    Singleton instance of `ConcurrentMainScheduler`
    */
    public static let instance = ConcurrentMainScheduler(mainScheduler: MainScheduler.instance)

    /**
    Schedules an action to be executed immediatelly.

    - parameter state: State passed to the action to be executed.
    - parameter action: Action to be executed.
    - returns: The disposable object used to cancel the scheduled action (best effort).
    */
    public func schedule<StateType>(state: StateType, action: (StateType) -> Disposable) -> Disposable {
        if NSThread.currentThread().isMainThread {
            return action(state)
        }

        let cancel = SingleAssignmentDisposable()

        dispatch_async(_mainQueue) {
            if cancel.disposed {
                return
            }

            cancel.disposable = action(state)
        }

        return cancel
    }

    /**
    Schedules an action to be executed.

    - parameter state: State passed to the action to be executed.
    - parameter dueTime: Relative time after which to execute the action.
    - parameter action: Action to be executed.
    - returns: The disposable object used to cancel the scheduled action (best effort).
    */
    public final func scheduleRelative<StateType>(state: StateType, dueTime: NSTimeInterval, action: (StateType) -> Disposable) -> Disposable {
        return _mainScheduler.scheduleRelative(state, dueTime: dueTime, action: action)
    }

    /**
    Schedules a periodic piece of work.

    - parameter state: State passed to the action to be executed.
    - parameter startAfter: Period after which initial work should be run.
    - parameter period: Period for running the work periodically.
    - parameter action: Action to be executed.
    - returns: The disposable object used to cancel the scheduled action (best effort).
    */
    public func schedulePeriodic<StateType>(state: StateType, startAfter: TimeInterval, period: TimeInterval, action: (StateType) -> StateType) -> Disposable {
        return _mainScheduler.schedulePeriodic(state, startAfter: startAfter, period: period, action: action)
    }
}