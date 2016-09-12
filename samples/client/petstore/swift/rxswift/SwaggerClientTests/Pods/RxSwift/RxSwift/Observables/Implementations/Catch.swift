//
//  Catch.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 4/19/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

// catch with callback

class CatchSinkProxy<O: ObserverType> : ObserverType {
    typealias E = O.E
    typealias Parent = CatchSink<O>

    private let _parent: Parent

    init(parent: Parent) {
        _parent = parent
    }

    func on(event: Event<E>) {
        _parent.forwardOn(event)

        switch event {
        case .Next:
            break
        case .Error, .Completed:
            _parent.dispose()
        }
    }
}

class CatchSink<O: ObserverType> : Sink<O>, ObserverType {
    typealias E = O.E
    typealias Parent = Catch<E>

    private let _parent: Parent
    private let _subscription = SerialDisposable()

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        let d1 = SingleAssignmentDisposable()
        _subscription.disposable = d1
        d1.disposable = _parent._source.subscribe(self)

        return _subscription
    }

    func on(event: Event<E>) {
        switch event {
        case .Next:
            forwardOn(event)
        case .Completed:
            forwardOn(event)
            dispose()
        case .Error(let error):
            do {
                let catchSequence = try _parent._handler(error)

                let observer = CatchSinkProxy(parent: self)

                _subscription.disposable = catchSequence.subscribe(observer)
            }
            catch let e {
                forwardOn(.Error(e))
                dispose()
            }
        }
    }
}

class Catch<Element> : Producer<Element> {
    typealias Handler = (ErrorType) throws -> Observable<Element>

    private let _source: Observable<Element>
    private let _handler: Handler

    init(source: Observable<Element>, handler: Handler) {
        _source = source
        _handler = handler
    }

    override func run<O: ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = CatchSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}

// catch enumerable

class CatchSequenceSink<S: SequenceType, O: ObserverType where S.Generator.Element : ObservableConvertibleType, S.Generator.Element.E == O.E>
    : TailRecursiveSink<S, O>
    , ObserverType {
    typealias Element = O.E
    typealias Parent = CatchSequence<S>

    private var _lastError: ErrorType?

    override init(observer: O) {
        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        switch event {
        case .Next:
            forwardOn(event)
        case .Error(let error):
            _lastError = error
            schedule(.MoveNext)
        case .Completed:
            forwardOn(event)
            dispose()
        }
    }

    override func subscribeToNext(source: Observable<E>) -> Disposable {
        return source.subscribe(self)
    }

    override func done() {
        if let lastError = _lastError {
            forwardOn(.Error(lastError))
        }
        else {
            forwardOn(.Completed)
        }

        self.dispose()
    }

    override func extract(observable: Observable<Element>) -> SequenceGenerator? {
        if let onError = observable as? CatchSequence<S> {
            return (onError.sources.generate(), nil)
        }
        else {
            return nil
        }
    }
}

class CatchSequence<S: SequenceType where S.Generator.Element : ObservableConvertibleType> : Producer<S.Generator.Element.E> {
    typealias Element = S.Generator.Element.E

    let sources: S

    init(sources: S) {
        self.sources = sources
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = CatchSequenceSink<S, O>(observer: observer)
        sink.disposable = sink.run((self.sources.generate(), nil))
        return sink
    }
}