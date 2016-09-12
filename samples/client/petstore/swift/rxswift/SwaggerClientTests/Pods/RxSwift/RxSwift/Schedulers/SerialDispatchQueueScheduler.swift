//
//  SerialDispatchQueueScheduler.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/8/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Abstracts the work that needs to be performed on a specific `dispatch_queue_t`. It will make sure
that even if concurrent dispatch queue is passed, it's transformed into a serial one.

It is extremely important that this scheduler is serial, because
certain operator perform optimizations that rely on that property.

Because there is no way of detecting is passed dispatch queue serial or
concurrent, for every queue that is being passed, worst case (concurrent)
will be assumed, and internal serial proxy dispatch queue will be created.

This scheduler can also be used with internal serial queue alone.

In case some customization need to be made on it before usage,
internal serial queue can be customized using `serialQueueConfiguration`
callback.
*/
public class SerialDispatchQueueScheduler: SchedulerType {
    public typealias TimeInterval = NSTimeInterval
    public typealias Time = NSDate

    private let _serialQueue : dispatch_queue_t

    /**
    - returns: Current time.
    */
    public var now : NSDate {
        return NSDate()
    }

    // leeway for scheduling timers
    private var _leeway: Int64 = 0

    init(serialQueue: dispatch_queue_t) {
        _serialQueue = serialQueue
    }

    /**
    Constructs new `SerialDispatchQueueScheduler` with internal serial queue named `internalSerialQueueName`.

    Additional dispatch queue properties can be set after dispatch queue is created using `serialQueueConfiguration`.

    - parameter internalSerialQueueName: Name of internal serial dispatch queue.
    - parameter serialQueueConfiguration: Additional configuration of internal serial dispatch queue.
    */
    public convenience init(internalSerialQueueName: String, serialQueueConfiguration: ((dispatch_queue_t) -> Void)? = nil) {
        let queue = dispatch_queue_create(internalSerialQueueName, DISPATCH_QUEUE_SERIAL)
        serialQueueConfiguration?(queue)
        self.init(serialQueue: queue)
    }

    /**
    Constructs new `SerialDispatchQueueScheduler` named `internalSerialQueueName` that wraps `queue`.

    - parameter queue: Possibly concurrent dispatch queue used to perform work.
    - parameter internalSerialQueueName: Name of internal serial dispatch queue proxy.
    */
    public convenience init(queue: dispatch_queue_t, internalSerialQueueName: String) {
        let serialQueue = dispatch_queue_create(internalSerialQueueName, DISPATCH_QUEUE_SERIAL)
        dispatch_set_target_queue(serialQueue, queue)
        self.init(serialQueue: serialQueue)
    }

    /**
     Constructs new `SerialDispatchQueueScheduler` that wraps on of the global concurrent dispatch queues.

     - parameter globalConcurrentQueueQOS: Identifier for global dispatch queue with specified quality of service class.
     - parameter internalSerialQueueName: Custom name for internal serial dispatch queue proxy.
     */
    @available(iOS 8, OSX 10.10, *)
    public convenience init(globalConcurrentQueueQOS: DispatchQueueSchedulerQOS, internalSerialQueueName: String = "rx.global_dispatch_queue.serial") {
        let priority = globalConcurrentQueueQOS.QOSClass
        self.init(queue: dispatch_get_global_queue(priority, UInt(0)), internalSerialQueueName: internalSerialQueueName)
    }

    class func convertTimeIntervalToDispatchInterval(timeInterval: NSTimeInterval) -> Int64 {
        return Int64(timeInterval * Double(NSEC_PER_SEC))
    }

    class func convertTimeIntervalToDispatchTime(timeInterval: NSTimeInterval) -> dispatch_time_t {
        return dispatch_time(DISPATCH_TIME_NOW, convertTimeIntervalToDispatchInterval(timeInterval))
    }

    /**
    Schedules an action to be executed immediately.

    - parameter state: State passed to the action to be executed.
    - parameter action: Action to be executed.
    - returns: The disposable object used to cancel the scheduled action (best effort).
    */
    public final func schedule<StateType>(state: StateType, action: (StateType) -> Disposable) -> Disposable {
        return self.scheduleInternal(state, action: action)
    }

    func scheduleInternal<StateType>(state: StateType, action: (StateType) -> Disposable) -> Disposable {
        let cancel = SingleAssignmentDisposable()

        dispatch_async(_serialQueue) {
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
        let timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, _serialQueue)

        let dispatchInterval = MainScheduler.convertTimeIntervalToDispatchTime(dueTime)

        let compositeDisposable = CompositeDisposable()

        dispatch_source_set_timer(timer, dispatchInterval, DISPATCH_TIME_FOREVER, 0)
        dispatch_source_set_event_handler(timer, {
            if compositeDisposable.disposed {
                return
            }
            compositeDisposable.addDisposable(action(state))
        })
        dispatch_resume(timer)

        compositeDisposable.addDisposable(AnonymousDisposable {
            dispatch_source_cancel(timer)
        })

        return compositeDisposable
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
        let timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, _serialQueue)

        let initial = MainScheduler.convertTimeIntervalToDispatchTime(startAfter)
        let dispatchInterval = MainScheduler.convertTimeIntervalToDispatchInterval(period)

        var timerState = state

        let validDispatchInterval = dispatchInterval < 0 ? 0 : UInt64(dispatchInterval)

        dispatch_source_set_timer(timer, initial, validDispatchInterval, 0)
        let cancel = AnonymousDisposable {
            dispatch_source_cancel(timer)
        }
        dispatch_source_set_event_handler(timer, {
            if cancel.disposed {
                return
            }
            timerState = action(timerState)
        })
        dispatch_resume(timer)

        return cancel
    }
}
