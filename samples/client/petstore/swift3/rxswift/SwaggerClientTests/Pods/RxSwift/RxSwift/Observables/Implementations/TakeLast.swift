//
//  TakeLast.swift
//  Rx
//
//  Created by Tomi Koskinen on 25/10/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation


class TakeLastSink<ElementType, O: ObserverType> : Sink<O>, ObserverType where O.E == ElementType {
    typealias Parent = TakeLast<ElementType>
    typealias E = ElementType
    
    private let _parent: Parent
    
    private var _elements: Queue<ElementType>
    
    init(parent: Parent, observer: O) {
        _parent = parent
        _elements = Queue<ElementType>(capacity: parent._count + 1)
        super.init(observer: observer)
    }
    
    func on(_ event: Event<E>) {
        switch event {
        case .next(let value):
            _elements.enqueue(value)
            if _elements.count > self._parent._count {
                let _ = _elements.dequeue()
            }
        case .error:
            forwardOn(event)
            dispose()
        case .completed:
            for e in _elements {
                forwardOn(.next(e))
            }
            forwardOn(.completed)
            dispose()
        }
    }
}

class TakeLast<Element>: Producer<Element> {
    fileprivate let _source: Observable<Element>
    fileprivate let _count: Int
    
    init(source: Observable<Element>, count: Int) {
        if count < 0 {
            rxFatalError("count can't be negative")
        }
        _source = source
        _count = count
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let sink = TakeLastSink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}
