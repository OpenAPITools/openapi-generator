//
//  SkipWhile.swift
//  Rx
//
//  Created by Yury Korolev on 10/9/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

class SkipWhileSink<ElementType, O: ObserverType where O.E == ElementType> : Sink<O>, ObserverType {

    typealias Parent = SkipWhile<ElementType>
    typealias Element = ElementType

    private let _parent: Parent
    private var _running = false

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        switch event {
        case .Next(let value):
            if !_running {
                do {
                    _running = try !_parent._predicate(value)
                } catch let e {
                    forwardOn(.Error(e))
                    dispose()
                    return
                }
            }

            if _running {
                forwardOn(.Next(value))
            }
        case .Error, .Completed:
            forwardOn(event)
            dispose()
        }
    }
}

class SkipWhileSinkWithIndex<ElementType, O: ObserverType where O.E == ElementType> : Sink<O>, ObserverType {

    typealias Parent = SkipWhile<ElementType>
    typealias Element = ElementType

    private let _parent: Parent
    private var _index = 0
    private var _running = false

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        switch event {
        case .Next(let value):
            if !_running {
                do {
                    _running = try !_parent._predicateWithIndex(value, _index)
                    try incrementChecked(&_index)
                } catch let e {
                    forwardOn(.Error(e))
                    dispose()
                    return
                }
            }

            if _running {
                forwardOn(.Next(value))
            }
        case .Error, .Completed:
            forwardOn(event)
            dispose()
        }
    }
}

class SkipWhile<Element>: Producer<Element> {
    typealias Predicate = (Element) throws -> Bool
    typealias PredicateWithIndex = (Element, Int) throws -> Bool

    private let _source: Observable<Element>
    private let _predicate: Predicate!
    private let _predicateWithIndex: PredicateWithIndex!

    init(source: Observable<Element>, predicate: Predicate) {
        _source = source
        _predicate = predicate
        _predicateWithIndex = nil
    }

    init(source: Observable<Element>, predicate: PredicateWithIndex) {
        _source = source
        _predicate = nil
        _predicateWithIndex = predicate
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        if let _ = _predicate {
            let sink = SkipWhileSink(parent: self, observer: observer)
            sink.disposable = _source.subscribe(sink)
            return sink
        }
        else {
            let sink = SkipWhileSinkWithIndex(parent: self, observer: observer)
            sink.disposable = _source.subscribe(sink)
            return sink
        }
    }
}
