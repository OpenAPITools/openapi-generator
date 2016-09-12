//
//  DistinctUntilChanged.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/15/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class DistinctUntilChangedSink<O: ObserverType, Key>: Sink<O>, ObserverType {
    typealias E = O.E

    private let _parent: DistinctUntilChanged<E, Key>
    private var _currentKey: Key? = nil

    init(parent: DistinctUntilChanged<E, Key>, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<E>) {
        switch event {
        case .Next(let value):
            do {
                let key = try _parent._selector(value)
                var areEqual = false
                if let currentKey = _currentKey {
                    areEqual = try _parent._comparer(currentKey, key)
                }

                if areEqual {
                    return
                }

                _currentKey = key

                forwardOn(event)
            }
            catch let error {
                forwardOn(.Error(error))
                dispose()
            }
        case .Error, .Completed:
            forwardOn(event)
            dispose()
        }
    }
}

class DistinctUntilChanged<Element, Key>: Producer<Element> {
    typealias KeySelector = (Element) throws -> Key
    typealias EqualityComparer = (Key, Key) throws -> Bool

    private let _source: Observable<Element>
    private let _selector: KeySelector
    private let _comparer: EqualityComparer

    init(source: Observable<Element>, selector: KeySelector, comparer: EqualityComparer) {
        _source = source
        _selector = selector
        _comparer = comparer
    }

    override func run<O: ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = DistinctUntilChangedSink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}