//
//  Skip.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/25/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

// count version

class SkipCountSink<ElementType, O: ObserverType> : Sink<O>, ObserverType where O.E == ElementType {
    typealias Parent = SkipCount<ElementType>
    typealias Element = ElementType
    
    let parent: Parent
    
    var remaining: Int
    
    init(parent: Parent, observer: O) {
        self.parent = parent
        self.remaining = parent.count
        super.init(observer: observer)
    }
    
    func on(_ event: Event<Element>) {
        switch event {
        case .next(let value):
            
            if remaining <= 0 {
                forwardOn(.next(value))
            }
            else {
                remaining -= 1
            }
        case .error:
            forwardOn(event)
            self.dispose()
        case .completed:
            forwardOn(event)
            self.dispose()
        }
    }
    
}

class SkipCount<Element>: Producer<Element> {
    let source: Observable<Element>
    let count: Int
    
    init(source: Observable<Element>, count: Int) {
        self.source = source
        self.count = count
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = SkipCountSink(parent: self, observer: observer)
        sink.disposable = source.subscribe(sink)

        return sink
    }
}

// time version

class SkipTimeSink<ElementType, O: ObserverType> : Sink<O>, ObserverType where O.E == ElementType {
    typealias Parent = SkipTime<ElementType>
    typealias Element = ElementType

    let parent: Parent
    
    // state
    var open = false
    
    init(parent: Parent, observer: O) {
        self.parent = parent
        super.init(observer: observer)
    }
    
    func on(_ event: Event<Element>) {
        switch event {
        case .next(let value):
            if open {
                forwardOn(.next(value))
            }
        case .error:
            forwardOn(event)
            self.dispose()
        case .completed:
            forwardOn(event)
            self.dispose()
        }
    }
    
    func tick() {
        open = true
    }
    
    func run() -> Disposable {
        let disposeTimer = parent.scheduler.scheduleRelative((), dueTime: self.parent.duration) {
            self.tick()
            return Disposables.create()
        }
        
        let disposeSubscription = parent.source.subscribe(self)
        
        return Disposables.create(disposeTimer, disposeSubscription)
    }
}

class SkipTime<Element>: Producer<Element> {
    let source: Observable<Element>
    let duration: RxTimeInterval
    let scheduler: SchedulerType
    
    init(source: Observable<Element>, duration: RxTimeInterval, scheduler: SchedulerType) {
        self.source = source
        self.scheduler = scheduler
        self.duration = duration
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = SkipTimeSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
