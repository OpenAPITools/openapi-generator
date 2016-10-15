//
//  Just.swift
//  Rx
//
//  Created by Krunoslav Zaher on 8/30/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class JustScheduledSink<O: ObserverType> : Sink<O> {
    typealias Parent = JustScheduled<O.E>

    private let _parent: Parent

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        let scheduler = _parent._scheduler
        return scheduler.schedule(_parent._element) { element in
            self.forwardOn(.next(element))
            return scheduler.schedule(()) { _ in
                self.forwardOn(.completed)
                return Disposables.create()
            }
        }
    }
}

class JustScheduled<Element> : Producer<Element> {
    fileprivate let _scheduler: ImmediateSchedulerType
    fileprivate let _element: Element

    init(element: Element, scheduler: ImmediateSchedulerType) {
        _scheduler = scheduler
        _element = element
    }

    override func subscribe<O : ObserverType>(_ observer: O) -> Disposable where O.E == E {
        let sink = JustScheduledSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}

class Just<Element> : Producer<Element> {
    private let _element: Element
    
    init(element: Element) {
        _element = element
    }
    
    override func subscribe<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        observer.on(.next(_element))
        observer.on(.completed)
        return Disposables.create()
    }
}
