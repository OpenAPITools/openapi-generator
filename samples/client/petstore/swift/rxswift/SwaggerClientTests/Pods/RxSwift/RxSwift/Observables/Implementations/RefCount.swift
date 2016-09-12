//
//  RefCount.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/5/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class RefCountSink<CO: ConnectableObservableType, O: ObserverType where CO.E == O.E>
    : Sink<O>
    , ObserverType {
    typealias Element = O.E
    typealias Parent = RefCount<CO>

    private let _parent: Parent

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        let subscription = _parent._source.subscribeSafe(self)

        _parent._lock.lock(); defer { _parent._lock.unlock() } // {
            if _parent._count == 0 {
                _parent._count = 1
                _parent._connectableSubscription = _parent._source.connect()
            }
            else {
                _parent._count = _parent._count + 1
            }
        // }

        return AnonymousDisposable {
            subscription.dispose()
            self._parent._lock.lock(); defer { self._parent._lock.unlock() } // {
                if self._parent._count == 1 {
                    self._parent._connectableSubscription!.dispose()
                    self._parent._count = 0
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

    func on(event: Event<Element>) {
        switch event {
        case .Next:
            forwardOn(event)
        case .Error, .Completed:
            forwardOn(event)
            dispose()
        }
    }
}

class RefCount<CO: ConnectableObservableType>: Producer<CO.E> {
    private let _lock = NSRecursiveLock()

    // state
    private var _count = 0
    private var _connectableSubscription = nil as Disposable?

    private let _source: CO

    init(source: CO) {
        _source = source
    }

    override func run<O: ObserverType where O.E == CO.E>(observer: O) -> Disposable {
        let sink = RefCountSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}