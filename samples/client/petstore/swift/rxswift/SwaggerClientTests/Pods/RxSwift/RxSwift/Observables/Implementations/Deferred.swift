//
//  Deferred.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 4/19/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class DeferredSink<S: ObservableType, O: ObserverType where S.E == O.E> : Sink<O>, ObserverType {
    typealias E = O.E

    private let _observableFactory: () throws -> S

    init(observableFactory: () throws -> S, observer: O) {
        _observableFactory = observableFactory
        super.init(observer: observer)
    }

    func run() -> Disposable {
        do {
            let result = try _observableFactory()
            return result.subscribe(self)
        }
        catch let e {
            forwardOn(.Error(e))
            dispose()
            return NopDisposable.instance
        }
    }

    func on(event: Event<E>) {
        forwardOn(event)

        switch event {
        case .Next:
            break
        case .Error:
            dispose()
        case .Completed:
            dispose()
        }
    }
}

class Deferred<S: ObservableType> : Producer<S.E> {
    typealias Factory = () throws -> S

    private let _observableFactory : Factory

    init(observableFactory: Factory) {
        _observableFactory = observableFactory
    }

    override func run<O: ObserverType where O.E == S.E>(observer: O) -> Disposable {
        let sink = DeferredSink(observableFactory: _observableFactory, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}