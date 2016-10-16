//
//  AnonymousObservable.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/8/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class AnonymousObservableSink<O: ObserverType> : Sink<O>, ObserverType {
    typealias E = O.E
    typealias Parent = AnonymousObservable<E>

    // state
    private var _isStopped: AtomicInt = 0

    override init(observer: O) {
        super.init(observer: observer)
    }

    func on(_ event: Event<E>) {
        switch event {
        case .next:
            if _isStopped == 1 {
                return
            }
            forwardOn(event)
        case .error, .completed:
            if AtomicCompareAndSwap(0, 1, &_isStopped) {
                forwardOn(event)
                dispose()
            }
        }
    }

    func run(_ parent: Parent) -> Disposable {
        return parent._subscribeHandler(AnyObserver(self))
    }
}

class AnonymousObservable<Element> : Producer<Element> {
    typealias SubscribeHandler = (AnyObserver<Element>) -> Disposable

    let _subscribeHandler: SubscribeHandler

    init(_ subscribeHandler: @escaping SubscribeHandler) {
        _subscribeHandler = subscribeHandler
    }

    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = AnonymousObservableSink(observer: observer)
        sink.disposable = sink.run(self)
        return sink
    }
}
