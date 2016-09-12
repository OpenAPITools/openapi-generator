//
//  Observable+Extensions.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/21/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

extension ObservableType {
    /**
    Subscribes an event handler to an observable sequence.

    - parameter on: Action to invoke for each event in the observable sequence.
    - returns: Subscription object used to unsubscribe from the observable sequence.
    */
    @warn_unused_result(message="http://git.io/rxs.ud")
    public func subscribe(on: (event: Event<E>) -> Void)
        -> Disposable {
        let observer = AnonymousObserver { e in
            on(event: e)
        }
        return self.subscribeSafe(observer)
    }

    /**
    Subscribes an element handler, an error handler, a completion handler and disposed handler to an observable sequence.

    - parameter onNext: Action to invoke for each element in the observable sequence.
    - parameter onError: Action to invoke upon errored termination of the observable sequence.
    - parameter onCompleted: Action to invoke upon graceful termination of the observable sequence.
    - parameter onDisposed: Action to invoke upon any type of termination of sequence (if the sequence has
        gracefully completed, errored, or if the generation is cancelled by disposing subscription).
    - returns: Subscription object used to unsubscribe from the observable sequence.
    */
    @warn_unused_result(message="http://git.io/rxs.ud")
    public func subscribe(onNext onNext: (E -> Void)? = nil, onError: (ErrorType -> Void)? = nil, onCompleted: (() -> Void)? = nil, onDisposed: (() -> Void)? = nil)
        -> Disposable {

        let disposable: Disposable

        if let disposed = onDisposed {
            disposable = AnonymousDisposable(disposed)
        }
        else {
            disposable = NopDisposable.instance
        }

        let observer = AnonymousObserver<E> { e in
            switch e {
            case .Next(let value):
                onNext?(value)
            case .Error(let e):
                onError?(e)
                disposable.dispose()
            case .Completed:
                onCompleted?()
                disposable.dispose()
            }
        }
        return BinaryDisposable(
            self.subscribeSafe(observer),
            disposable
        )
    }

    /**
    Subscribes an element handler to an observable sequence.

    - parameter onNext: Action to invoke for each element in the observable sequence.
    - returns: Subscription object used to unsubscribe from the observable sequence.
    */
    @warn_unused_result(message="http://git.io/rxs.ud")
    public func subscribeNext(onNext: (E) -> Void)
        -> Disposable {
        let observer = AnonymousObserver<E> { e in
            if case .Next(let value) = e {
                onNext(value)
            }
        }
        return self.subscribeSafe(observer)
    }

    /**
    Subscribes an error handler to an observable sequence.

    - parameter onError: Action to invoke upon errored termination of the observable sequence.
    - returns: Subscription object used to unsubscribe from the observable sequence.
    */
    @warn_unused_result(message="http://git.io/rxs.ud")
    public func subscribeError(onError: (ErrorType) -> Void)
        -> Disposable {
        let observer = AnonymousObserver<E> { e in
            if case .Error(let error) = e {
                onError(error)
            }
        }
        return self.subscribeSafe(observer)
    }

    /**
    Subscribes a completion handler to an observable sequence.

    - parameter onCompleted: Action to invoke upon graceful termination of the observable sequence.
    - returns: Subscription object used to unsubscribe from the observable sequence.
    */
    @warn_unused_result(message="http://git.io/rxs.ud")
    public func subscribeCompleted(onCompleted: () -> Void)
        -> Disposable {
        let observer = AnonymousObserver<E> { e in
            if case .Completed = e {
                onCompleted()
            }
        }
        return self.subscribeSafe(observer)
    }
}

public extension ObservableType {
    /**
    All internal subscribe calls go through this method.
    */
    @warn_unused_result(message="http://git.io/rxs.ud")
    func subscribeSafe<O: ObserverType where O.E == E>(observer: O) -> Disposable {
        return self.asObservable().subscribe(observer)
    }
}
