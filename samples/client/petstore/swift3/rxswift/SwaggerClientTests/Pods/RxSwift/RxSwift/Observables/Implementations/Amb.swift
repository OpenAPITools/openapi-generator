//
//  Amb.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/14/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

enum AmbState {
    case neither
    case left
    case right
}

class AmbObserver<ElementType, O: ObserverType> : ObserverType where O.E == ElementType {
    typealias Element = ElementType
    typealias Parent = AmbSink<ElementType, O>
    typealias This = AmbObserver<ElementType, O>
    typealias Sink = (This, Event<Element>) -> Void
    
    fileprivate let _parent: Parent
    fileprivate var _sink: Sink
    fileprivate var _cancel: Disposable
    
    init(parent: Parent, cancel: Disposable, sink: @escaping Sink) {
#if TRACE_RESOURCES
        let _ = AtomicIncrement(&resourceCount)
#endif
        
        _parent = parent
        _sink = sink
        _cancel = cancel
    }
    
    func on(_ event: Event<Element>) {
        _sink(self, event)
        if event.isStopEvent {
            _cancel.dispose()
        }
    }
    
    deinit {
#if TRACE_RESOURCES
        let _ = AtomicDecrement(&resourceCount)
#endif
    }
}

class AmbSink<ElementType, O: ObserverType> : Sink<O> where O.E == ElementType {
    typealias Parent = Amb<ElementType>
    typealias AmbObserverType = AmbObserver<ElementType, O>

    private let _parent: Parent
    
    private let _lock = NSRecursiveLock()
    // state
    private var _choice = AmbState.neither
    
    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }
    
    func run() -> Disposable {
        let subscription1 = SingleAssignmentDisposable()
        let subscription2 = SingleAssignmentDisposable()
        let disposeAll = Disposables.create(subscription1, subscription2)
        
        let forwardEvent = { (o: AmbObserverType, event: Event<ElementType>) -> Void in
            self.forwardOn(event)
        }
        
        let decide = { (o: AmbObserverType, event: Event<ElementType>, me: AmbState, otherSubscription: Disposable) in
            self._lock.performLocked {
                if self._choice == .neither {
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
            decide(o, e, .left, subscription2)
        }
        
        let sink2 = AmbObserver(parent: self, cancel: subscription1) { o, e in
            decide(o, e, .right, subscription1)
        }
        
        subscription1.disposable = _parent._left.subscribe(sink1)
        subscription2.disposable = _parent._right.subscribe(sink2)
        
        return disposeAll
    }
}

class Amb<Element>: Producer<Element> {
    fileprivate let _left: Observable<Element>
    fileprivate let _right: Observable<Element>
    
    init(left: Observable<Element>, right: Observable<Element>) {
        _left = left
        _right = right
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = AmbSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
