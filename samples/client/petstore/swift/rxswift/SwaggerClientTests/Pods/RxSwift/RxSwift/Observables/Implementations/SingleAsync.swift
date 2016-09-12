//
//  SingleAsync.swift
//  Rx
//
//  Created by Junior B. on 09/11/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class SingleAsyncSink<ElementType, O: ObserverType where O.E == ElementType> : Sink<O>, ObserverType {
    typealias Parent = SingleAsync<ElementType>
    typealias E = ElementType

    private let _parent: Parent
    private var _seenValue: Bool = false

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<E>) {
        switch event {
        case .Next(let value):
            do {
                let forward = try _parent._predicate?(value) ?? true
                if !forward {
                    return
                }
            }
            catch let error {
                forwardOn(.Error(error as ErrorType))
                dispose()
                return
            }

            if _seenValue == false {
                _seenValue = true
                forwardOn(.Next(value))
            } else {
                forwardOn(.Error(RxError.MoreThanOneElement))
                dispose()
            }

        case .Error:
            forwardOn(event)
            dispose()
        case .Completed:
            if (!_seenValue) {
                forwardOn(.Error(RxError.NoElements))
            } else {
                forwardOn(.Completed)
            }
            dispose()
        }
    }
}

class SingleAsync<Element>: Producer<Element> {
    typealias Predicate = (Element) throws -> Bool

    private let _source: Observable<Element>
    private let _predicate: Predicate?

    init(source: Observable<Element>, predicate: Predicate? = nil) {
        _source = source
        _predicate = predicate
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = SingleAsyncSink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}