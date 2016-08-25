//
//  TakeWhile.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/7/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class TakeWhileSink<ElementType, O: ObserverType where O.E == ElementType>
    : Sink<O>
    , ObserverType {
    typealias Parent = TakeWhile<ElementType>
    typealias Element = ElementType

    private let _parent: Parent

    private var _running = true

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        switch event {
        case .Next(let value):
            if !_running {
                return
            }

            do {
                _running = try _parent._predicate(value)
            } catch let e {
                forwardOn(.Error(e))
                dispose()
                return
            }

            if _running {
                forwardOn(.Next(value))
            } else {
                forwardOn(.Completed)
                dispose()
            }
        case .Error, .Completed:
            forwardOn(event)
            dispose()
        }
    }

}

class TakeWhileSinkWithIndex<ElementType, O: ObserverType where O.E == ElementType>
    : Sink<O>
    , ObserverType {
    typealias Parent = TakeWhile<ElementType>
    typealias Element = ElementType

    private let _parent: Parent

    private var _running = true
    private var _index = 0

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        switch event {
        case .Next(let value):
            if !_running {
                return
            }

            do {
                _running = try _parent._predicateWithIndex(value, _index)
                try incrementChecked(&_index)
            } catch let e {
                forwardOn(.Error(e))
                dispose()
                return
            }

            if _running {
                forwardOn(.Next(value))
            } else {
                forwardOn(.Completed)
                dispose()
            }
        case .Error, .Completed:
            forwardOn(event)
            dispose()
        }
    }

}

class TakeWhile<Element>: Producer<Element> {
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
            let sink = TakeWhileSink(parent: self, observer: observer)
            sink.disposable = _source.subscribe(sink)
            return sink
        } else {
            let sink = TakeWhileSinkWithIndex(parent: self, observer: observer)
            sink.disposable = _source.subscribe(sink)
            return sink
        }
    }
}