//
//  RecursiveScheduler.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/7/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Type erased recursive scheduler.
*/
class AnyRecursiveScheduler<State> {
    typealias Action =  (state: State, scheduler: AnyRecursiveScheduler<State>) -> Void

    private let _lock = NSRecursiveLock()

    // state
    private let _group = CompositeDisposable()

    private var _scheduler: SchedulerType
    private var _action: Action?

    init(scheduler: SchedulerType, action: Action) {
        _action = action
        _scheduler = scheduler
    }

    /**
    Schedules an action to be executed recursively.

    - parameter state: State passed to the action to be executed.
    - parameter dueTime: Relative time after which to execute the recursive action.
    */
    func schedule(state: State, dueTime: RxTimeInterval) {

        var isAdded = false
        var isDone = false

        var removeKey: CompositeDisposable.DisposeKey? = nil
        let d = _scheduler.scheduleRelative(state, dueTime: dueTime) { (state) -> Disposable in
            // best effort
            if self._group.disposed {
                return NopDisposable.instance
            }

            let action = self._lock.calculateLocked { () -> Action? in
                if isAdded {
                    self._group.removeDisposable(removeKey!)
                }
                else {
                    isDone = true
                }

                return self._action
            }

            if let action = action {
                action(state: state, scheduler: self)
            }

            return NopDisposable.instance
        }

        _lock.performLocked {
            if !isDone {
                removeKey = _group.addDisposable(d)
                isAdded = true
            }
        }
    }

    /**
    Schedules an action to be executed recursively.

    - parameter state: State passed to the action to be executed.
    */
    func schedule(state: State) {

        var isAdded = false
        var isDone = false

        var removeKey: CompositeDisposable.DisposeKey? = nil
        let d = _scheduler.schedule(state) { (state) -> Disposable in
            // best effort
            if self._group.disposed {
                return NopDisposable.instance
            }

            let action = self._lock.calculateLocked { () -> Action? in
                if isAdded {
                    self._group.removeDisposable(removeKey!)
                }
                else {
                    isDone = true
                }

                return self._action
            }

            if let action = action {
                action(state: state, scheduler: self)
            }

            return NopDisposable.instance
        }

        _lock.performLocked {
            if !isDone {
                removeKey = _group.addDisposable(d)
                isAdded = true
            }
        }
    }

    func dispose() {
        _lock.performLocked {
            _action = nil
        }
        _group.dispose()
    }
}

/**
Type erased recursive scheduler.
*/
class RecursiveImmediateScheduler<State> {
    typealias Action =  (state: State, recurse: State -> Void) -> Void

    private var _lock = SpinLock()
    private let _group = CompositeDisposable()

    private var _action: Action?
    private let _scheduler: ImmediateSchedulerType

    init(action: Action, scheduler: ImmediateSchedulerType) {
        _action = action
        _scheduler = scheduler
    }

    // immediate scheduling

    /**
    Schedules an action to be executed recursively.

    - parameter state: State passed to the action to be executed.
    */
    func schedule(state: State) {

        var isAdded = false
        var isDone = false

        var removeKey: CompositeDisposable.DisposeKey? = nil
        let d = _scheduler.schedule(state) { (state) -> Disposable in
            // best effort
            if self._group.disposed {
                return NopDisposable.instance
            }

            let action = self._lock.calculateLocked { () -> Action? in
                if isAdded {
                    self._group.removeDisposable(removeKey!)
                }
                else {
                    isDone = true
                }

                return self._action
            }

            if let action = action {
                action(state: state, recurse: self.schedule)
            }

            return NopDisposable.instance
        }

        _lock.performLocked {
            if !isDone {
                removeKey = _group.addDisposable(d)
                isAdded = true
            }
        }
    }

    func dispose() {
        _lock.performLocked {
            _action = nil
        }
        _group.dispose()
    }
}