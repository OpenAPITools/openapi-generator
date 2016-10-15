//
//  Sequence.swift
//  Rx
//
//  Created by Krunoslav Zaher on 11/14/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class ObservableSequenceSink<S: Sequence, O: ObserverType> : Sink<O> where S.Iterator.Element == O.E {
    typealias Parent = ObservableSequence<S>

    private let _parent: Parent

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        return _parent._scheduler.scheduleRecursive((_parent._elements.makeIterator(), _parent._elements)) { (iterator, recurse) in
            var mutableIterator = iterator
            if let next = mutableIterator.0.next() {
                self.forwardOn(.next(next))
                recurse(mutableIterator)
            }
            else {
                self.forwardOn(.completed)
            }
        }
    }
}

class ObservableSequence<S: Sequence> : Producer<S.Iterator.Element> {
    fileprivate let _elements: S
    fileprivate let _scheduler: ImmediateSchedulerType

    init(elements: S, scheduler: ImmediateSchedulerType) {
        _elements = elements
        _scheduler = scheduler
    }

    override func subscribe<O : ObserverType>(_ observer: O) -> Disposable where O.E == E {
        let sink = ObservableSequenceSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
