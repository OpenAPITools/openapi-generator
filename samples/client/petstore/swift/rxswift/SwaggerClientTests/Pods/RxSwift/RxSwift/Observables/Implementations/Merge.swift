//
//  Merge.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/28/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

// MARK: Limited concurrency version

class MergeLimitedSinkIter<S: ObservableConvertibleType, O: ObserverType where S.E == O.E>
    : ObserverType
    , LockOwnerType
    , SynchronizedOnType {
    typealias E = O.E
    typealias DisposeKey = Bag<Disposable>.KeyType
    typealias Parent = MergeLimitedSink<S, O>

    private let _parent: Parent
    private let _disposeKey: DisposeKey

    var _lock: NSRecursiveLock {
        return _parent._lock
    }

    init(parent: Parent, disposeKey: DisposeKey) {
        _parent = parent
        _disposeKey = disposeKey
    }

    func on(event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(event: Event<E>) {
        switch event {
        case .Next:
            _parent.forwardOn(event)
        case .Error:
            _parent.forwardOn(event)
            _parent.dispose()
        case .Completed:
            _parent._group.removeDisposable(_disposeKey)
            if let next = _parent._queue.dequeue() {
                _parent.subscribe(next, group: _parent._group)
            }
            else {
                _parent._activeCount = _parent._activeCount - 1

                if _parent._stopped && _parent._activeCount == 0 {
                    _parent.forwardOn(.Completed)
                    _parent.dispose()
                }
            }
        }
    }
}

class MergeLimitedSink<S: ObservableConvertibleType, O: ObserverType where S.E == O.E>
    : Sink<O>
    , ObserverType
    , LockOwnerType
    , SynchronizedOnType {
    typealias E = S
    typealias QueueType = Queue<S>

    private let _maxConcurrent: Int

    let _lock = NSRecursiveLock()

    // state
    private var _stopped = false
    private var _activeCount = 0
    private var _queue = QueueType(capacity: 2)

    private let _sourceSubscription = SingleAssignmentDisposable()
    private let _group = CompositeDisposable()

    init(maxConcurrent: Int, observer: O) {
        _maxConcurrent = maxConcurrent

        _group.addDisposable(_sourceSubscription)
        super.init(observer: observer)
    }

    func run(source: Observable<S>) -> Disposable {
        _group.addDisposable(_sourceSubscription)

        let disposable = source.subscribe(self)
        _sourceSubscription.disposable = disposable
        return _group
    }

    func subscribe(innerSource: E, group: CompositeDisposable) {
        let subscription = SingleAssignmentDisposable()

        let key = group.addDisposable(subscription)

        if let key = key {
            let observer = MergeLimitedSinkIter(parent: self, disposeKey: key)

            let disposable = innerSource.asObservable().subscribe(observer)
            subscription.disposable = disposable
        }
    }

    func on(event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(event: Event<E>) {
        switch event {
        case .Next(let value):
            let subscribe: Bool
            if _activeCount < _maxConcurrent {
                _activeCount += 1
                subscribe = true
            }
            else {
                _queue.enqueue(value)
                subscribe = false
            }

            if subscribe {
                self.subscribe(value, group: _group)
            }
        case .Error(let error):
            forwardOn(.Error(error))
            dispose()
        case .Completed:
            if _activeCount == 0 {
                forwardOn(.Completed)
                dispose()
            }
            else {
                _sourceSubscription.dispose()
            }

            _stopped = true
        }
    }
}

class MergeLimited<S: ObservableConvertibleType> : Producer<S.E> {
    private let _source: Observable<S>
    private let _maxConcurrent: Int

    init(source: Observable<S>, maxConcurrent: Int) {
        _source = source
        _maxConcurrent = maxConcurrent
    }

    override func run<O: ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = MergeLimitedSink<S, O>(maxConcurrent: _maxConcurrent, observer: observer)
        sink.disposable = sink.run(_source)
        return sink
    }
}

// MARK: Merge

final class MergeBasicSink<S: ObservableConvertibleType, O: ObserverType where O.E == S.E> : MergeSink<S, S, O> {
    override init(observer: O) {
        super.init(observer: observer)
    }

    override func performMap(element: S) throws -> S {
        return element
    }
}

// MARK: flatMap

final class FlatMapSink<SourceType, S: ObservableConvertibleType, O: ObserverType where O.E == S.E> : MergeSink<SourceType, S, O> {
    typealias Selector = (SourceType) throws -> S

    private let _selector: Selector

    init(selector: Selector, observer: O) {
        _selector = selector
        super.init(observer: observer)
    }

    override func performMap(element: SourceType) throws -> S {
        return try _selector(element)
    }
}

final class FlatMapWithIndexSink<SourceType, S: ObservableConvertibleType, O: ObserverType where O.E == S.E> : MergeSink<SourceType, S, O> {
    typealias Selector = (SourceType, Int) throws -> S

    private var _index = 0
    private let _selector: Selector

    init(selector: Selector, observer: O) {
        _selector = selector
        super.init(observer: observer)
    }

    override func performMap(element: SourceType) throws -> S {
        return try _selector(element, try incrementChecked(&_index))
    }
}

// MARK: FlatMapFirst

final class FlatMapFirstSink<SourceType, S: ObservableConvertibleType, O: ObserverType where O.E == S.E> : MergeSink<SourceType, S, O> {
    typealias Selector = (SourceType) throws -> S

    private let _selector: Selector

    override var subscribeNext: Bool {
        return _group.count == MergeNoIterators
    }

    init(selector: Selector, observer: O) {
        _selector = selector
        super.init(observer: observer)
    }

    override func performMap(element: SourceType) throws -> S {
        return try _selector(element)
    }
}

// It's value is one because initial source subscription is always in CompositeDisposable
private let MergeNoIterators = 1

class MergeSinkIter<SourceType, S: ObservableConvertibleType, O: ObserverType where O.E == S.E> : ObserverType {
    typealias Parent = MergeSink<SourceType, S, O>
    typealias DisposeKey = CompositeDisposable.DisposeKey
    typealias E = O.E

    private let _parent: Parent
    private let _disposeKey: DisposeKey

    init(parent: Parent, disposeKey: DisposeKey) {
        _parent = parent
        _disposeKey = disposeKey
    }

    func on(event: Event<E>) {
        switch event {
        case .Next(let value):
            _parent._lock.lock(); defer { _parent._lock.unlock() } // lock {
                _parent.forwardOn(.Next(value))
            // }
        case .Error(let error):
            _parent._lock.lock(); defer { _parent._lock.unlock() } // lock {
                _parent.forwardOn(.Error(error))
                _parent.dispose()
            // }
        case .Completed:
            _parent._group.removeDisposable(_disposeKey)
            // If this has returned true that means that `Completed` should be sent.
            // In case there is a race who will sent first completed,
            // lock will sort it out. When first Completed message is sent
            // it will set observer to nil, and thus prevent further complete messages
            // to be sent, and thus preserving the sequence grammar.
            if _parent._stopped && _parent._group.count == MergeNoIterators {
                _parent._lock.lock(); defer { _parent._lock.unlock() } // lock {
                    _parent.forwardOn(.Completed)
                    _parent.dispose()
                // }
            }
        }
    }
}


class MergeSink<SourceType, S: ObservableConvertibleType, O: ObserverType where O.E == S.E>
    : Sink<O>
    , ObserverType {
    typealias ResultType = O.E
    typealias Element = SourceType

    private let _lock = NSRecursiveLock()

    private var subscribeNext: Bool {
        return true
    }

    // state
    private let _group = CompositeDisposable()
    private let _sourceSubscription = SingleAssignmentDisposable()

    private var _stopped = false

    override init(observer: O) {
        super.init(observer: observer)
    }

    func performMap(element: SourceType) throws -> S {
        abstractMethod()
    }

    func on(event: Event<SourceType>) {
        switch event {
        case .Next(let element):
            if !subscribeNext {
                return
            }
            do {
                let value = try performMap(element)
                subscribeInner(value.asObservable())
            }
            catch let e {
                forwardOn(.Error(e))
                dispose()
            }
        case .Error(let error):
            _lock.lock(); defer { _lock.unlock() } // lock {
                forwardOn(.Error(error))
                dispose()
            // }
        case .Completed:
            _lock.lock(); defer { _lock.unlock() } // lock {
                _stopped = true
                if _group.count == MergeNoIterators {
                    forwardOn(.Completed)
                    dispose()
                }
                else {
                    _sourceSubscription.dispose()
                }
            //}
        }
    }

    func subscribeInner(source: Observable<O.E>) {
        let iterDisposable = SingleAssignmentDisposable()
        if let disposeKey = _group.addDisposable(iterDisposable) {
            let iter = MergeSinkIter(parent: self, disposeKey: disposeKey)
            let subscription = source.subscribe(iter)
            iterDisposable.disposable = subscription
        }
    }

    func run(source: Observable<SourceType>) -> Disposable {
        _group.addDisposable(_sourceSubscription)

        let subscription = source.subscribe(self)
        _sourceSubscription.disposable = subscription

        return _group
    }
}

// MARK: Producers

final class FlatMap<SourceType, S: ObservableConvertibleType>: Producer<S.E> {
    typealias Selector = (SourceType) throws -> S

    private let _source: Observable<SourceType>

    private let _selector: Selector

    init(source: Observable<SourceType>, selector: Selector) {
        _source = source
        _selector = selector
    }

    override func run<O: ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = FlatMapSink(selector: _selector, observer: observer)
        sink.disposable = sink.run(_source)
        return sink
    }
}

final class FlatMapWithIndex<SourceType, S: ObservableConvertibleType>: Producer<S.E> {
    typealias Selector = (SourceType, Int) throws -> S

    private let _source: Observable<SourceType>

    private let _selector: Selector

    init(source: Observable<SourceType>, selector: Selector) {
        _source = source
        _selector = selector
    }

    override func run<O: ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = FlatMapWithIndexSink<SourceType, S, O>(selector: _selector, observer: observer)
        sink.disposable = sink.run(_source)
        return sink
    }

}

final class FlatMapFirst<SourceType, S: ObservableConvertibleType>: Producer<S.E> {
    typealias Selector = (SourceType) throws -> S

    private let _source: Observable<SourceType>

    private let _selector: Selector

    init(source: Observable<SourceType>, selector: Selector) {
        _source = source
        _selector = selector
    }

    override func run<O: ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = FlatMapFirstSink<SourceType, S, O>(selector: _selector, observer: observer)
        sink.disposable = sink.run(_source)
        return sink
    }
}

final class Merge<S: ObservableConvertibleType> : Producer<S.E> {
    private let _source: Observable<S>

    init(source: Observable<S>) {
        _source = source
    }

    override func run<O: ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = MergeBasicSink<S, O>(observer: observer)
        sink.disposable = sink.run(_source)
        return sink
    }
}
