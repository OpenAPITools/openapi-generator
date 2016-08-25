//
//  Do.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/21/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class DoSink<O: ObserverType> : Sink<O>, ObserverType {
    typealias Element = O.E
    typealias Parent = Do<Element>

    private let _parent: Parent

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        do {
            try _parent._eventHandler(event)
            forwardOn(event)
            if event.isStopEvent {
                dispose()
            }
        }
        catch let error {
            forwardOn(.Error(error))
            dispose()
        }
    }
}

class Do<Element> : Producer<Element> {
    typealias EventHandler = Event<Element> throws -> Void

    private let _source: Observable<Element>
    private let _eventHandler: EventHandler

    init(source: Observable<Element>, eventHandler: EventHandler) {
        _source = source
        _eventHandler = eventHandler
    }

    override func run<O: ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = DoSink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}