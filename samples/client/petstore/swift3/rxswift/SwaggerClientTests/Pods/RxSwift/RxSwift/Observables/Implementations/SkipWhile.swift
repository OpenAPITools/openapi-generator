//
//  SkipWhile.swift
//  Rx
//
//  Created by Yury Korolev on 10/9/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

class SkipWhileSink<ElementType, O: ObserverType> : Sink<O>, ObserverType where O.E == ElementType {

    typealias Parent = SkipWhile<ElementType>
    typealias Element = ElementType

    fileprivate let _parent: Parent
    fileprivate var _running = false

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(_ event: Event<Element>) {
        switch event {
        case .next(let value):
            if !_running {
                do {
                    _running = try !_parent._predicate(value)
                } catch let e {
                    forwardOn(.error(e))
                    dispose()
                    return
                }
            }

            if _running {
                forwardOn(.next(value))
            }
        case .error, .completed:
            forwardOn(event)
            dispose()
        }
    }
}

class SkipWhileSinkWithIndex<ElementType, O: ObserverType> : Sink<O>, ObserverType where O.E == ElementType {

    typealias Parent = SkipWhile<ElementType>
    typealias Element = ElementType

    fileprivate let _parent: Parent
    fileprivate var _index = 0
    fileprivate var _running = false

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(_ event: Event<Element>) {
        switch event {
        case .next(let value):
            if !_running {
                do {
                    _running = try !_parent._predicateWithIndex(value, _index)
                    let _ = try incrementChecked(&_index)
                } catch let e {
                    forwardOn(.error(e))
                    dispose()
                    return
                }
            }

            if _running {
                forwardOn(.next(value))
            }
        case .error, .completed:
            forwardOn(event)
            dispose()
        }
    }
}

class SkipWhile<Element>: Producer<Element> {
    typealias Predicate = (Element) throws -> Bool
    typealias PredicateWithIndex = (Element, Int) throws -> Bool

    fileprivate let _source: Observable<Element>
    fileprivate let _predicate: Predicate!
    fileprivate let _predicateWithIndex: PredicateWithIndex!

    init(source: Observable<Element>, predicate: @escaping Predicate) {
        _source = source
        _predicate = predicate
        _predicateWithIndex = nil
    }

    init(source: Observable<Element>, predicate: @escaping PredicateWithIndex) {
        _source = source
        _predicate = nil
        _predicateWithIndex = predicate
    }

    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
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
