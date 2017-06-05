//
//  Multicast.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 2/27/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

extension ObservableType {
    
    /**
    Multicasts the source sequence notifications through an instantiated subject into all uses of the sequence within a selector function. 
    
    Each subscription to the resulting sequence causes a separate multicast invocation, exposing the sequence resulting from the selector function's invocation.

    For specializations with fixed subject types, see `publish` and `replay`.

    - seealso: [multicast operator on reactivex.io](http://reactivex.io/documentation/operators/publish.html)
    
    - parameter subjectSelector: Factory function to create an intermediate subject through which the source sequence's elements will be multicast to the selector function.
    - parameter selector: Selector function which can use the multicasted source sequence subject to the policies enforced by the created subject.
    - returns: An observable sequence that contains the elements of a sequence produced by multicasting the source sequence within a selector function.
    */
    public func multicast<S: SubjectType, R>(_ subjectSelector: @escaping () throws -> S, selector: @escaping (Observable<S.E>) throws -> Observable<R>)
        -> Observable<R> where S.SubjectObserverType.E == E {
        return Multicast(
            source: self.asObservable(),
            subjectSelector: subjectSelector,
            selector: selector
        )
    }
}

extension ObservableType {
    
    /**
    Returns a connectable observable sequence that shares a single subscription to the underlying sequence. 
    
    This operator is a specialization of `multicast` using a `PublishSubject`.

    - seealso: [publish operator on reactivex.io](http://reactivex.io/documentation/operators/publish.html)
    
    - returns: A connectable observable sequence that shares a single subscription to the underlying sequence.
    */
    public func publish() -> ConnectableObservable<E> {
        return self.multicast(PublishSubject())
    }
}

extension ObservableType {

    /**
     Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying bufferSize elements.

     This operator is a specialization of `multicast` using a `ReplaySubject`.

     - seealso: [replay operator on reactivex.io](http://reactivex.io/documentation/operators/replay.html)

     - parameter bufferSize: Maximum element count of the replay buffer.
     - returns: A connectable observable sequence that shares a single subscription to the underlying sequence.
     */
    public func replay(_ bufferSize: Int)
        -> ConnectableObservable<E> {
        return self.multicast(ReplaySubject.create(bufferSize: bufferSize))
    }

    /**
     Returns a connectable observable sequence that shares a single subscription to the underlying sequence replaying all elements.

     This operator is a specialization of `multicast` using a `ReplaySubject`.

     - seealso: [replay operator on reactivex.io](http://reactivex.io/documentation/operators/replay.html)

     - returns: A connectable observable sequence that shares a single subscription to the underlying sequence.
     */
    public func replayAll()
        -> ConnectableObservable<E> {
        return self.multicast(ReplaySubject.createUnbounded())
    }
}

extension ConnectableObservableType {
    
    /**
    Returns an observable sequence that stays connected to the source as long as there is at least one subscription to the observable sequence.

    - seealso: [refCount operator on reactivex.io](http://reactivex.io/documentation/operators/refCount.html)
    
    - returns: An observable sequence that stays connected to the source as long as there is at least one subscription to the observable sequence.
    */
    public func refCount() -> Observable<E> {
        return RefCount(source: self)
    }
}

extension ObservableType {

    /**
     Returns an observable sequence that shares a single subscription to the underlying sequence.

     This operator is a specialization of publish which creates a subscription when the number of observers goes from zero to one, then shares that subscription with all subsequent observers until the number of observers returns to zero, at which point the subscription is disposed.

     - seealso: [share operator on reactivex.io](http://reactivex.io/documentation/operators/refcount.html)

     - returns: An observable sequence that contains the elements of a sequence produced by multicasting the source sequence.
     */
    public func share() -> Observable<E> {
        return self.publish().refCount()
    }
}

final fileprivate class RefCountSink<CO: ConnectableObservableType, O: ObserverType>
    : Sink<O>
    , ObserverType where CO.E == O.E {
    typealias Element = O.E
    typealias Parent = RefCount<CO>

    private let _parent: Parent

    init(parent: Parent, observer: O, cancel: Cancelable) {
        _parent = parent
        super.init(observer: observer, cancel: cancel)
    }

    func run() -> Disposable {
        let subscription = _parent._source.subscribe(self)

        _parent._lock.lock(); defer { _parent._lock.unlock() } // {
        if _parent._count == 0 {
            _parent._count = 1
            _parent._connectableSubscription = _parent._source.connect()
        }
        else {
            _parent._count = _parent._count + 1
        }
        // }

        return Disposables.create {
            subscription.dispose()
            self._parent._lock.lock(); defer { self._parent._lock.unlock() } // {
            if self._parent._count == 1 {
                self._parent._count = 0
                guard let connectableSubscription = self._parent._connectableSubscription else {
                    return
                }

                connectableSubscription.dispose()
                self._parent._connectableSubscription = nil
            }
            else if self._parent._count > 1 {
                self._parent._count = self._parent._count - 1
            }
            else {
                rxFatalError("Something went wrong with RefCount disposing mechanism")
            }
            // }
        }
    }

    func on(_ event: Event<Element>) {
        switch event {
        case .next:
            forwardOn(event)
        case .error, .completed:
            forwardOn(event)
            dispose()
        }
    }
}

final fileprivate class RefCount<CO: ConnectableObservableType>: Producer<CO.E> {
    fileprivate let _lock = RecursiveLock()

    // state
    fileprivate var _count = 0
    fileprivate var _connectableSubscription = nil as Disposable?

    fileprivate let _source: CO

    init(source: CO) {
        _source = source
    }

    override func run<O: ObserverType>(_ observer: O, cancel: Cancelable) -> (sink: Disposable, subscription: Disposable) where O.E == CO.E {
        let sink = RefCountSink(parent: self, observer: observer, cancel: cancel)
        let subscription = sink.run()
        return (sink: sink, subscription: subscription)
    }
}

final fileprivate class MulticastSink<S: SubjectType, O: ObserverType>: Sink<O>, ObserverType {
    typealias Element = O.E
    typealias ResultType = Element
    typealias MutlicastType = Multicast<S, O.E>
    
    private let _parent: MutlicastType
    
    init(parent: MutlicastType, observer: O, cancel: Cancelable) {
        _parent = parent
        super.init(observer: observer, cancel: cancel)
    }
    
    func run() -> Disposable {
        do {
            let subject = try _parent._subjectSelector()
            let connectable = ConnectableObservableAdapter(source: _parent._source, subject: subject)
            
            let observable = try _parent._selector(connectable)
            
            let subscription = observable.subscribe(self)
            let connection = connectable.connect()
                
            return Disposables.create(subscription, connection)
        }
        catch let e {
            forwardOn(.error(e))
            dispose()
            return Disposables.create()
        }
    }
    
    func on(_ event: Event<ResultType>) {
        forwardOn(event)
        switch event {
            case .next: break
            case .error, .completed:
                dispose()
        }
    }
}

final fileprivate class Multicast<S: SubjectType, R>: Producer<R> {
    typealias SubjectSelectorType = () throws -> S
    typealias SelectorType = (Observable<S.E>) throws -> Observable<R>
    
    fileprivate let _source: Observable<S.SubjectObserverType.E>
    fileprivate let _subjectSelector: SubjectSelectorType
    fileprivate let _selector: SelectorType
    
    init(source: Observable<S.SubjectObserverType.E>, subjectSelector: @escaping SubjectSelectorType, selector: @escaping SelectorType) {
        _source = source
        _subjectSelector = subjectSelector
        _selector = selector
    }
    
    override func run<O: ObserverType>(_ observer: O, cancel: Cancelable) -> (sink: Disposable, subscription: Disposable) where O.E == R {
        let sink = MulticastSink(parent: self, observer: observer, cancel: cancel)
        let subscription = sink.run()
        return (sink: sink, subscription: subscription)
    }
}
