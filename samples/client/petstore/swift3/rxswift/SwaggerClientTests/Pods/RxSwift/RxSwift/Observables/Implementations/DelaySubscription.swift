//
//  DelaySubscription.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/14/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class DelaySubscriptionSink<ElementType, O: ObserverType>
    : Sink<O>
    , ObserverType where O.E == ElementType {
    typealias Parent = DelaySubscription<ElementType>
    typealias E = O.E
    
    private let _parent: Parent
    
    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }
    
    func on(_ event: Event<E>) {
        forwardOn(event)
        if event.isStopEvent {
            dispose()
        }
    }
    
}

class DelaySubscription<Element>: Producer<Element> {
    private let _source: Observable<Element>
    private let _dueTime: RxTimeInterval
    private let _scheduler: SchedulerType
    
    init(source: Observable<Element>, dueTime: RxTimeInterval, scheduler: SchedulerType) {
        _source = source
        _dueTime = dueTime
        _scheduler = scheduler
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = DelaySubscriptionSink(parent: self, observer: observer)
        sink.disposable = _scheduler.scheduleRelative((), dueTime: _dueTime) { _ in
            return self._source.subscribe(sink)
        }

        return sink
    }
}
