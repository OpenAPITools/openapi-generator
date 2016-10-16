//
//  TakeUntil.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/7/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class TakeUntilSinkOther<ElementType, Other, O: ObserverType>
    : ObserverType
    , LockOwnerType
    , SynchronizedOnType where O.E == ElementType {
    typealias Parent = TakeUntilSink<ElementType, Other, O>
    typealias E = Other
    
    fileprivate let _parent: Parent

    var _lock: NSRecursiveLock {
        return _parent._lock
    }
    
    fileprivate let _subscription = SingleAssignmentDisposable()
    
    init(parent: Parent) {
        _parent = parent
#if TRACE_RESOURCES
        let _ = AtomicIncrement(&resourceCount)
#endif
    }
    
    func on(_ event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(_ event: Event<E>) {
        switch event {
        case .next:
            _parent.forwardOn(.completed)
            _parent.dispose()
        case .error(let e):
            _parent.forwardOn(.error(e))
            _parent.dispose()
        case .completed:
            _parent._open = true
            _subscription.dispose()
        }
    }
    
#if TRACE_RESOURCES
    deinit {
        let _ = AtomicDecrement(&resourceCount)
    }
#endif
}

class TakeUntilSink<ElementType, Other, O: ObserverType>
    : Sink<O>
    , LockOwnerType
    , ObserverType
    , SynchronizedOnType where O.E == ElementType {
    typealias E = ElementType
    typealias Parent = TakeUntil<E, Other>
    
    fileprivate let _parent: Parent
 
    let _lock = NSRecursiveLock()
    
    // state
    fileprivate var _open = false
    
    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }
    
    func on(_ event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(_ event: Event<E>) {
        switch event {
        case .next:
            forwardOn(event)
        case .error:
            forwardOn(event)
            dispose()
        case .completed:
            forwardOn(event)
            dispose()
        }
    }
    
    func run() -> Disposable {
        let otherObserver = TakeUntilSinkOther(parent: self)
        let otherSubscription = _parent._other.subscribe(otherObserver)
        otherObserver._subscription.disposable = otherSubscription
        let sourceSubscription = _parent._source.subscribe(self)
        
        return Disposables.create(sourceSubscription, otherObserver._subscription)
    }
}

class TakeUntil<Element, Other>: Producer<Element> {
    
    fileprivate let _source: Observable<Element>
    fileprivate let _other: Observable<Other>
    
    init(source: Observable<Element>, other: Observable<Other>) {
        _source = source
        _other = other
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = TakeUntilSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
