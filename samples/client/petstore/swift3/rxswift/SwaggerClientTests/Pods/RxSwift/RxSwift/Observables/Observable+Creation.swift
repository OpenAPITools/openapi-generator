//
//  Observable+Creation.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/21/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

extension Observable {
    // MARK: create

    /**
    Creates an observable sequence from a specified subscribe method implementation.

    - seealso: [create operator on reactivex.io](http://reactivex.io/documentation/operators/create.html)

    - parameter subscribe: Implementation of the resulting observable sequence's `subscribe` method.
    - returns: The observable sequence with the specified implementation for the `subscribe` method.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func create(_ subscribe: @escaping (AnyObserver<E>) -> Disposable) -> Observable<E> {
        return AnonymousObservable(subscribe)
    }

    // MARK: empty

    /**
    Returns an empty observable sequence, using the specified scheduler to send out the single `Completed` message.

    - seealso: [empty operator on reactivex.io](http://reactivex.io/documentation/operators/empty-never-throw.html)

    - returns: An observable sequence with no elements.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func empty() -> Observable<E> {
        return Empty<E>()
    }

    // MARK: never

    /**
    Returns a non-terminating observable sequence, which can be used to denote an infinite duration.

    - seealso: [never operator on reactivex.io](http://reactivex.io/documentation/operators/empty-never-throw.html)

    - returns: An observable sequence whose observers will never get called.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func never() -> Observable<E> {
        return Never()
    }

    // MARK: just

    /**
    Returns an observable sequence that contains a single element.

    - seealso: [just operator on reactivex.io](http://reactivex.io/documentation/operators/just.html)

    - parameter element: Single element in the resulting observable sequence.
    - parameter: Scheduler to send the single element on.
    - returns: An observable sequence containing the single specified element.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func just(_ element: E, scheduler: ImmediateSchedulerType? = nil) -> Observable<E> {
        if let scheduler = scheduler {
            return JustScheduled(element: element, scheduler: scheduler)
        }
        else {
            return Just(element: element)
        }
    }

    // MARK: fail

    /**
    Returns an observable sequence that terminates with an `error`.

    - seealso: [throw operator on reactivex.io](http://reactivex.io/documentation/operators/empty-never-throw.html)

    - returns: The observable sequence that terminates with specified error.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func error(_ error: Swift.Error) -> Observable<E> {
        return Error(error: error)
    }

    // MARK: of

    /**
    This method creates a new Observable instance with a variable number of elements.

    - seealso: [from operator on reactivex.io](http://reactivex.io/documentation/operators/from.html)

    - parameter elements: Elements to generate.
    - parameter scheduler: Scheduler to send elements on. If `nil`, elements are sent immediatelly on subscription.
    - returns: The observable sequence whose elements are pulled from the given arguments.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func of(_ elements: E ..., scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance) -> Observable<E> {
        return ObservableSequence(elements: elements, scheduler: scheduler)
    }

    // MARK: defer

    /**
    Returns an observable sequence that invokes the specified factory function whenever a new observer subscribes.

    - seealso: [defer operator on reactivex.io](http://reactivex.io/documentation/operators/defer.html)

    - parameter observableFactory: Observable factory function to invoke for each observer that subscribes to the resulting sequence.
    - returns: An observable sequence whose observers trigger an invocation of the given observable factory function.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func deferred(_ observableFactory: @escaping () throws -> Observable<E>)
        -> Observable<E> {
        return Deferred(observableFactory: observableFactory)
    }

    /**
    Generates an observable sequence by running a state-driven loop producing the sequence's elements, using the specified scheduler
    to run the loop send out observer messages.

    - seealso: [create operator on reactivex.io](http://reactivex.io/documentation/operators/create.html)

    - parameter initialState: Initial state.
    - parameter condition: Condition to terminate generation (upon returning `false`).
    - parameter iterate: Iteration step function.
    - parameter scheduler: Scheduler on which to run the generator loop.
    - returns: The generated sequence.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func generate(initialState: E, condition: @escaping (E) throws -> Bool, scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance, iterate: @escaping (E) throws -> E) -> Observable<E> {
        return Generate(initialState: initialState, condition: condition, iterate: iterate, resultSelector: { $0 }, scheduler: scheduler)
    }

    /**
    Generates an observable sequence that repeats the given element infinitely, using the specified scheduler to send out observer messages.

    - seealso: [repeat operator on reactivex.io](http://reactivex.io/documentation/operators/repeat.html)

    - parameter element: Element to repeat.
    - parameter scheduler: Scheduler to run the producer loop on.
    - returns: An observable sequence that repeats the given element infinitely.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func repeatElement(_ element: E, scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance) -> Observable<E> {
        return RepeatElement(element: element, scheduler: scheduler)
    }

    /**
    Constructs an observable sequence that depends on a resource object, whose lifetime is tied to the resulting observable sequence's lifetime.

    - seealso: [using operator on reactivex.io](http://reactivex.io/documentation/operators/using.html)
     
    - parameter resourceFactory: Factory function to obtain a resource object.
    - parameter observableFactory: Factory function to obtain an observable sequence that depends on the obtained resource.
    - returns: An observable sequence whose lifetime controls the lifetime of the dependent resource object.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func using<R: Disposable>(_ resourceFactory: @escaping () throws -> R, observableFactory: @escaping (R) throws -> Observable<E>) -> Observable<E> {
        return Using(resourceFactory: resourceFactory, observableFactory: observableFactory)
    }
}

extension Observable where Element : SignedInteger {
    /**
    Generates an observable sequence of integral numbers within a specified range, using the specified scheduler to generate and send out observer messages.

    - seealso: [range operator on reactivex.io](http://reactivex.io/documentation/operators/range.html)

    - parameter start: The value of the first integer in the sequence.
    - parameter count: The number of sequential integers to generate.
    - parameter scheduler: Scheduler to run the generator loop on.
    - returns: An observable sequence that contains a range of sequential integral numbers.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    public static func range(start: E, count: E, scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance) -> Observable<E> {
        return RangeProducer<E>(start: start, count: count, scheduler: scheduler)
    }
}

extension Sequence {
    /**
    Converts a sequence to an observable sequence.

    - seealso: [from operator on reactivex.io](http://reactivex.io/documentation/operators/from.html)

    - returns: The observable sequence whose elements are pulled from the given enumerable sequence.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    @available(*, deprecated, renamed: "Observable.from()")
    public func toObservable(_ scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance) -> Observable<Iterator.Element> {
        return ObservableSequence(elements: Array(self), scheduler: scheduler)
    }
}

extension Array {
    /**
    Converts a sequence to an observable sequence.

    - seealso: [from operator on reactivex.io](http://reactivex.io/documentation/operators/from.html)

    - returns: The observable sequence whose elements are pulled from the given enumerable sequence.
    */
    // @warn_unused_result(message:"http://git.io/rxs.uo")
    @available(*, deprecated, renamed: "Observable.from()")
    public func toObservable(_ scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance) -> Observable<Iterator.Element> {
        return ObservableSequence(elements: self, scheduler: scheduler)
    }
}

extension Observable {
    /**
     Converts an array to an observable sequence.

     - seealso: [from operator on reactivex.io](http://reactivex.io/documentation/operators/from.html)

     - returns: The observable sequence whose elements are pulled from the given enumerable sequence.
     */
    public static func from(_ array: [E], scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance) -> Observable<E> {
        return ObservableSequence(elements: array, scheduler: scheduler)
    }

    /**
     Converts a sequence to an observable sequence.

     - seealso: [from operator on reactivex.io](http://reactivex.io/documentation/operators/from.html)

     - returns: The observable sequence whose elements are pulled from the given enumerable sequence.
     */
    public static func from<S: Sequence>(_ sequence: S, scheduler: ImmediateSchedulerType = CurrentThreadScheduler.instance) -> Observable<E> where S.Iterator.Element == E {
        return ObservableSequence(elements: sequence, scheduler: scheduler)
    }
}
