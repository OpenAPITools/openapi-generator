//
//  SkipUntil.swift
//  Rx
//
//  Created by Yury Korolev on 10/3/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class SkipUntilSinkOther<ElementType, Other, O: ObserverType>
    : ObserverType
    , LockOwnerType
    , SynchronizedOnType where O.E == ElementType {
    typealias Parent = SkipUntilSink<ElementType, Other, O>
    typealias E = Other
    
    fileprivate let _parent: Parent

    var _lock: NSRecursiveLock {
        return _parent._lock
    }
    
    let _subscription = SingleAssignmentDisposable()

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
            _parent._forwardElements = true
            _subscription.dispose()
        case .error(let e):
            _parent.forwardOn(.error(e))
            _parent.dispose()
        case .completed:
            _subscription.dispose()
        }
    }
    
    #if TRACE_RESOURCES
    deinit {
        let _ = AtomicDecrement(&resourceCount)
    }
    #endif

}


class SkipUntilSink<ElementType, Other, O: ObserverType>
    : Sink<O>
    , ObserverType
    , LockOwnerType
    , SynchronizedOnType where O.E == ElementType {
    typealias E = ElementType
    typealias Parent = SkipUntil<E, Other>
    
    let _lock = NSRecursiveLock()
    fileprivate let _parent: Parent
    fileprivate var _forwardElements = false
    
    fileprivate let _sourceSubscription = SingleAssignmentDisposable()

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
            if _forwardElements {
                forwardOn(event)
            }
        case .error:
            forwardOn(event)
            dispose()
        case .completed:
            if _forwardElements {
                forwardOn(event)
            }
            _sourceSubscription.dispose()
        }
    }
    
    func run() -> Disposable {
        let sourceSubscription = _parent._source.subscribe(self)
        let otherObserver = SkipUntilSinkOther(parent: self)
        let otherSubscription = _parent._other.subscribe(otherObserver)
        _sourceSubscription.disposable = sourceSubscription
        otherObserver._subscription.disposable = otherSubscription
        
        return Disposables.create(_sourceSubscription, otherObserver._subscription)
    }
}

class SkipUntil<Element, Other>: Producer<Element> {
    
    fileprivate let _source: Observable<Element>
    fileprivate let _other: Observable<Other>
    
    init(source: Observable<Element>, other: Observable<Other>) {
        _source = source
        _other = other
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = SkipUntilSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
