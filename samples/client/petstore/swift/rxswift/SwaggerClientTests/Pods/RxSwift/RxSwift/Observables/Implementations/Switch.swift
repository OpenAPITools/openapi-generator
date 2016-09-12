//
//  Switch.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/12/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class SwitchSink<SourceType, S: ObservableConvertibleType, O: ObserverType where S.E == O.E>
    : Sink<O>
    , ObserverType
    , LockOwnerType
    , SynchronizedOnType {
    typealias E = SourceType

    private let _subscriptions: SingleAssignmentDisposable = SingleAssignmentDisposable()
    private let _innerSubscription: SerialDisposable = SerialDisposable()

    let _lock = NSRecursiveLock()

    // state
    private var _stopped = false
    private var _latest = 0
    private var _hasLatest = false

    override init(observer: O) {
        super.init(observer: observer)
    }

    func run(source: Observable<SourceType>) -> Disposable {
        let subscription = source.subscribe(self)
        _subscriptions.disposable = subscription
        return StableCompositeDisposable.create(_subscriptions, _innerSubscription)
    }

    func on(event: Event<E>) {
        synchronizedOn(event)
    }

    func performMap(element: SourceType) throws -> S {
        abstractMethod()
    }

    func _synchronized_on(event: Event<E>) {
        switch event {
        case .Next(let element):
            do {
                let observable = try performMap(element).asObservable()
                _hasLatest = true
                _latest = _latest &+ 1
                let latest = _latest

                let d = SingleAssignmentDisposable()
                _innerSubscription.disposable = d

                let observer = SwitchSinkIter(parent: self, id: latest, _self: d)
                let disposable = observable.subscribe(observer)
                d.disposable = disposable
            }
            catch let error {
                forwardOn(.Error(error))
                dispose()
            }
        case .Error(let error):
            forwardOn(.Error(error))
            dispose()
        case .Completed:
            _stopped = true

            _subscriptions.dispose()

            if !_hasLatest {
                forwardOn(.Completed)
                dispose()
            }
        }
    }
}

class SwitchSinkIter<SourceType, S: ObservableConvertibleType, O: ObserverType where S.E == O.E>
    : ObserverType
    , LockOwnerType
    , SynchronizedOnType {
    typealias E = S.E
    typealias Parent = SwitchSink<SourceType, S, O>

    private let _parent: Parent
    private let _id: Int
    private let _self: Disposable

    var _lock: NSRecursiveLock {
        return _parent._lock
    }

    init(parent: Parent, id: Int, _self: Disposable) {
        _parent = parent
        _id = id
        self._self = _self
    }

    func on(event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(event: Event<E>) {
        switch event {
        case .Next: break
        case .Error, .Completed:
            _self.dispose()
        }

        if _parent._latest != _id {
            return
        }

        switch event {
        case .Next:
            _parent.forwardOn(event)
        case .Error:
            _parent.forwardOn(event)
            _parent.dispose()
        case .Completed:
            _parent._hasLatest = false
            if _parent._stopped {
                _parent.forwardOn(event)
                _parent.dispose()
            }
        }
    }
}

// MARK: Specializations

final class SwitchIdentitySink<S: ObservableConvertibleType, O: ObserverType where O.E == S.E> : SwitchSink<S, S, O> {
    override init(observer: O) {
        super.init(observer: observer)
    }

    override func performMap(element: S) throws -> S {
        return element
    }
}

final class MapSwitchSink<SourceType, S: ObservableConvertibleType, O: ObserverType where O.E == S.E> : SwitchSink<SourceType, S, O> {
    typealias Selector = SourceType throws -> S

    private let _selector: Selector

    init(selector: Selector, observer: O) {
        _selector = selector
        super.init(observer: observer)
    }

    override func performMap(element: SourceType) throws -> S {
        return try _selector(element)
    }
}

// MARK: Producers

final class Switch<S: ObservableConvertibleType> : Producer<S.E> {
    private let _source: Observable<S>

    init(source: Observable<S>) {
        _source = source
    }

    override func run<O : ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = SwitchIdentitySink<S, O>(observer: observer)
        sink.disposable = sink.run(_source)
        return sink
    }
}

final class FlatMapLatest<SourceType, S: ObservableConvertibleType> : Producer<S.E> {
    typealias Selector = SourceType throws -> S

    private let _source: Observable<SourceType>
    private let _selector: Selector

    init(source: Observable<SourceType>, selector: Selector) {
        _source = source
        _selector = selector
    }

    override func run<O : ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = MapSwitchSink<SourceType, S, O>(selector: _selector, observer: observer)
        sink.disposable = sink.run(_source)
        return sink
    }
}