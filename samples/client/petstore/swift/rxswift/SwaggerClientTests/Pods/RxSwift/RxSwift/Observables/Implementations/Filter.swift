//
//  Filter.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/17/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class FilterSink<O : ObserverType>: Sink<O>, ObserverType {
    typealias Predicate = (Element) throws -> Bool
    typealias Element = O.E

    typealias Parent = Filter<Element>

    private let _predicate: Predicate

    init(predicate: Predicate, observer: O) {
        _predicate = predicate
        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        switch event {
            case .Next(let value):
                do {
                    let satisfies = try _predicate(value)
                    if satisfies {
                        forwardOn(.Next(value))
                    }
                }
                catch let e {
                    forwardOn(.Error(e))
                    dispose()
                }
            case .Completed, .Error:
                forwardOn(event)
                dispose()
        }
    }
}

class Filter<Element> : Producer<Element> {
    typealias Predicate = (Element) throws -> Bool

    private let _source: Observable<Element>
    private let _predicate: Predicate

    init(source: Observable<Element>, predicate: Predicate) {
        _source = source
        _predicate = predicate
    }

    override func run<O: ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = FilterSink(predicate: _predicate, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}