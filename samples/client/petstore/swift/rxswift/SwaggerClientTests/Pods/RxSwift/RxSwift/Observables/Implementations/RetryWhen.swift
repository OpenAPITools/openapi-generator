//
//  RetryWhen.swift
//  Rx
//
//  Created by Junior B. on 06/10/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class RetryTriggerSink<S: SequenceType, O: ObserverType, TriggerObservable: ObservableType, Error where S.Generator.Element : ObservableType, S.Generator.Element.E == O.E>
    : ObserverType {
    typealias E = TriggerObservable.E

    typealias Parent = RetryWhenSequenceSinkIter<S, O, TriggerObservable, Error>

    private let _parent: Parent

    init(parent: Parent) {
        _parent = parent
    }

    func on(event: Event<E>) {
        switch event {
        case .Next:
            _parent._parent._lastError = nil
            _parent._parent.schedule(.MoveNext)
        case .Error(let e):
            _parent._parent.forwardOn(.Error(e))
            _parent._parent.dispose()
        case .Completed:
            _parent._parent.forwardOn(.Completed)
            _parent._parent.dispose()
        }
    }
}

class RetryWhenSequenceSinkIter<S: SequenceType, O: ObserverType, TriggerObservable: ObservableType, Error where S.Generator.Element : ObservableType, S.Generator.Element.E == O.E>
    : SingleAssignmentDisposable
    , ObserverType {
    typealias E = O.E
    typealias Parent = RetryWhenSequenceSink<S, O, TriggerObservable, Error>

    private let _parent: Parent
    private let _errorHandlerSubscription = SingleAssignmentDisposable()

    init(parent: Parent) {
        _parent = parent
    }

    func on(event: Event<E>) {
        switch event {
        case .Next:
            _parent.forwardOn(event)
        case .Error(let error):
            _parent._lastError = error

            if let failedWith = error as? Error {
                // dispose current subscription
                super.dispose()

                let errorHandlerSubscription = _parent._notifier.subscribe(RetryTriggerSink(parent: self))
                _errorHandlerSubscription.disposable = errorHandlerSubscription
                _parent._errorSubject.on(.Next(failedWith))
            }
            else {
                _parent.forwardOn(.Error(error))
                _parent.dispose()
            }
        case .Completed:
            _parent.forwardOn(event)
            _parent.dispose()
        }
    }

    override func dispose() {
        super.dispose()
        _errorHandlerSubscription.dispose()
    }
}

class RetryWhenSequenceSink<S: SequenceType, O: ObserverType, TriggerObservable: ObservableType, Error where S.Generator.Element : ObservableType, S.Generator.Element.E == O.E>
    : TailRecursiveSink<S, O> {
    typealias Element = O.E
    typealias Parent = RetryWhenSequence<S, TriggerObservable, Error>

    let _lock = NSRecursiveLock()

    private let _parent: Parent

    private var _lastError: ErrorType?
    private let _errorSubject = PublishSubject<Error>()
    private let _handler: Observable<TriggerObservable.E>
    private let _notifier = PublishSubject<TriggerObservable.E>()

    init(parent: Parent, observer: O) {
        _parent = parent
        _handler = parent._notificationHandler(_errorSubject).asObservable()
        super.init(observer: observer)
    }

    override func done() {
        if let lastError = _lastError {
            forwardOn(.Error(lastError))
            _lastError = nil
        }
        else {
            forwardOn(.Completed)
        }

        dispose()
    }

    override func extract(observable: Observable<E>) -> SequenceGenerator? {
        // It is important to always return `nil` here because there are sideffects in the `run` method
        // that are dependant on particular `retryWhen` operator so single operator stack can't be reused in this
        // case.
        return nil
    }

    override func subscribeToNext(source: Observable<E>) -> Disposable {
        let iter = RetryWhenSequenceSinkIter(parent: self)
        iter.disposable = source.subscribe(iter)
        return iter
    }

    override func run(sources: SequenceGenerator) -> Disposable {
        let triggerSubscription = _handler.subscribe(_notifier.asObserver())
        let superSubscription = super.run(sources)
        return StableCompositeDisposable.create(superSubscription, triggerSubscription)
    }
}

class RetryWhenSequence<S: SequenceType, TriggerObservable: ObservableType, Error where S.Generator.Element : ObservableType> : Producer<S.Generator.Element.E> {
    typealias Element = S.Generator.Element.E

    private let _sources: S
    private let _notificationHandler: Observable<Error> -> TriggerObservable

    init(sources: S, notificationHandler: Observable<Error> -> TriggerObservable) {
        _sources = sources
        _notificationHandler = notificationHandler
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = RetryWhenSequenceSink<S, O, TriggerObservable, Error>(parent: self, observer: observer)
        sink.disposable = sink.run((self._sources.generate(), nil))
        return sink
    }
}