//
//  Repeat.swift
//  RxExample
//
//  Created by Krunoslav Zaher on 9/13/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class RepeatElement<Element> : Producer<Element> {
    fileprivate let _element: Element
    fileprivate let _scheduler: ImmediateSchedulerType
    
    init(element: Element, scheduler: ImmediateSchedulerType) {
        _element = element
        _scheduler = scheduler
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = RepeatElementSink(parent: self, observer: observer)
        sink.disposable = sink.run()

        return sink
    }
}

class RepeatElementSink<O: ObserverType> : Sink<O> {
    typealias Parent = RepeatElement<O.E>
    
    private let _parent: Parent
    
    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }
    
    func run() -> Disposable {
        return _parent._scheduler.scheduleRecursive(_parent._element) { e, recurse in
            self.forwardOn(.next(e))
            recurse(e)
        }
    }
}
