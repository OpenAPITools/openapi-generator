//
//  Take.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/12/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

// count version

class TakeCountSink<ElementType, O: ObserverType> : Sink<O>, ObserverType where O.E == ElementType {
    typealias Parent = TakeCount<ElementType>
    typealias E = ElementType
    
    private let _parent: Parent
    
    private var _remaining: Int
    
    init(parent: Parent, observer: O) {
        _parent = parent
        _remaining = parent._count
        super.init(observer: observer)
    }
    
    func on(_ event: Event<E>) {
        switch event {
        case .next(let value):
            
            if _remaining > 0 {
                _remaining -= 1
                
                forwardOn(.next(value))
            
                if _remaining == 0 {
                    forwardOn(.completed)
                    dispose()
                }
            }
        case .error:
            forwardOn(event)
            dispose()
        case .completed:
            forwardOn(event)
            dispose()
        }
    }
    
}

class TakeCount<Element>: Producer<Element> {
    fileprivate let _source: Observable<Element>
    fileprivate let _count: Int
    
    init(source: Observable<Element>, count: Int) {
        if count < 0 {
            rxFatalError("count can't be negative")
        }
        _source = source
        _count = count
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = TakeCountSink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}

// time version

class TakeTimeSink<ElementType, O: ObserverType>
    : Sink<O>
    , LockOwnerType
    , ObserverType
    , SynchronizedOnType where O.E == ElementType {
    typealias Parent = TakeTime<ElementType>
    typealias E = ElementType

    fileprivate let _parent: Parent
    
    let _lock = NSRecursiveLock()
    
    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }
    
    func on(_ event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(_ event: Event<E>) {
        switch event {
        case .next(let value):
            forwardOn(.next(value))
        case .error:
            forwardOn(event)
            dispose()
        case .completed:
            forwardOn(event)
            dispose()
        }
    }
    
    func tick() {
        _lock.lock(); defer { _lock.unlock() }

        forwardOn(.completed)
        dispose()
    }
    
    func run() -> Disposable {
        let disposeTimer = _parent._scheduler.scheduleRelative((), dueTime: _parent._duration) {
            self.tick()
            return Disposables.create()
        }
        
        let disposeSubscription = _parent._source.subscribe(self)
        
        return Disposables.create(disposeTimer, disposeSubscription)
    }
}

class TakeTime<Element> : Producer<Element> {
    typealias TimeInterval = RxTimeInterval
    
    fileprivate let _source: Observable<Element>
    fileprivate let _duration: TimeInterval
    fileprivate let _scheduler: SchedulerType
    
    init(source: Observable<Element>, duration: TimeInterval, scheduler: SchedulerType) {
        _source = source
        _scheduler = scheduler
        _duration = duration
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = TakeTimeSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
