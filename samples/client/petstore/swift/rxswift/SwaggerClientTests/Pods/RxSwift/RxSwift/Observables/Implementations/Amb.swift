//
//  Amb.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/14/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

enum AmbState {
    case Neither
    case Left
    case Right
}

class AmbObserver<ElementType, O: ObserverType where O.E == ElementType> : ObserverType {
    typealias Element = ElementType
    typealias Parent = AmbSink<ElementType, O>
    typealias This = AmbObserver<ElementType, O>
    typealias Sink = (This, Event<Element>) -> Void

    private let _parent: Parent
    private var _sink: Sink
    private var _cancel: Disposable

    init(parent: Parent, cancel: Disposable, sink: Sink) {
#if TRACE_RESOURCES
        AtomicIncrement(&resourceCount)
#endif

        _parent = parent
        _sink = sink
        _cancel = cancel
    }

    func on(event: Event<Element>) {
        _sink(self, event)
        if event.isStopEvent {
            _cancel.dispose()
        }
    }

    deinit {
#if TRACE_RESOURCES
        AtomicDecrement(&resourceCount)
#endif
    }
}

class AmbSink<ElementType, O: ObserverType where O.E == ElementType> : Sink<O> {
    typealias Parent = Amb<ElementType>
    typealias AmbObserverType = AmbObserver<ElementType, O>

    private let _parent: Parent

    private let _lock = NSRecursiveLock()
    // state
    private var _choice = AmbState.Neither

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        let subscription1 = SingleAssignmentDisposable()
        let subscription2 = SingleAssignmentDisposable()
        let disposeAll = StableCompositeDisposable.create(subscription1, subscription2)

        let forwardEvent = { (o: AmbObserverType, event: Event<ElementType>) -> Void in
            self.forwardOn(event)
        }

        let decide = { (o: AmbObserverType, event: Event<ElementType>, me: AmbState, otherSubscription: Disposable) in
            self._lock.performLocked {
                if self._choice == .Neither {
                    self._choice = me
                    o._sink = forwardEvent
                    o._cancel = disposeAll
                    otherSubscription.dispose()
                }

                if self._choice == me {
                    self.forwardOn(event)
                    if event.isStopEvent {
                        self.dispose()
                    }
                }
            }
        }

        let sink1 = AmbObserver(parent: self, cancel: subscription1) { o, e in
            decide(o, e, .Left, subscription2)
        }

        let sink2 = AmbObserver(parent: self, cancel: subscription1) { o, e in
            decide(o, e, .Right, subscription1)
        }

        subscription1.disposable = _parent._left.subscribe(sink1)
        subscription2.disposable = _parent._right.subscribe(sink2)

        return disposeAll
    }
}

class Amb<Element>: Producer<Element> {
    private let _left: Observable<Element>
    private let _right: Observable<Element>

    init(left: Observable<Element>, right: Observable<Element>) {
        _left = left
        _right = right
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = AmbSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}