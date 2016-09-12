//
//  Repeat.swift
//  RxExample
//
//  Created by Krunoslav Zaher on 9/13/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class RepeatElement<Element> : Producer<Element> {
    private let _element: Element
    private let _scheduler: ImmediateSchedulerType

    init(element: Element, scheduler: ImmediateSchedulerType) {
        _element = element
        _scheduler = scheduler
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
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
            self.forwardOn(.Next(e))
            recurse(e)
        }
    }
}