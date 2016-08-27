//
//  ImmediateSchedulerType.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 5/31/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents an object that immediately schedules units of work.
*/
public protocol ImmediateSchedulerType {
    /**
    Schedules an action to be executed immediately.

    - parameter state: State passed to the action to be executed.
    - parameter action: Action to be executed.
    - returns: The disposable object used to cancel the scheduled action (best effort).
    */
    func schedule<StateType>(state: StateType, action: (StateType) -> Disposable) -> Disposable
}

extension ImmediateSchedulerType {
    /**
    Schedules an action to be executed recursively.

    - parameter state: State passed to the action to be executed.
    - parameter action: Action to execute recursively. The last parameter passed to the action is used to trigger recursive scheduling of the action, passing in recursive invocation state.
    - returns: The disposable object used to cancel the scheduled action (best effort).
    */
    public func scheduleRecursive<State>(state: State, action: (state: State, recurse: (State) -> ()) -> ()) -> Disposable {
        let recursiveScheduler = RecursiveImmediateScheduler(action: action, scheduler: self)

        recursiveScheduler.schedule(state)

        return AnonymousDisposable(recursiveScheduler.dispose)
    }
}
