//
//  Sequence.swift
//  Rx
//
//  Created by Krunoslav Zaher on 11/14/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class SequenceSink<O: ObserverType> : Sink<O> {
    typealias Parent = Sequence<O.E>

    private let _parent: Parent

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        return _parent._scheduler!.scheduleRecursive((0, _parent._elements)) { (state, recurse) in
            if state.0 < state.1.count {
                self.forwardOn(.Next(state.1[state.0]))
                recurse((state.0 + 1, state.1))
            }
            else {
                self.forwardOn(.Completed)
            }
        }
    }
}

class Sequence<E> : Producer<E> {
    private let _elements: [E]
    private let _scheduler: ImmediateSchedulerType?

    init(elements: [E], scheduler: ImmediateSchedulerType?) {
        _elements = elements
        _scheduler = scheduler
    }

    override func subscribe<O : ObserverType where O.E == E>(observer: O) -> Disposable {
        // optimized version without scheduler
        guard _scheduler != nil else {
            for element in _elements {
                observer.on(.Next(element))
            }

            observer.on(.Completed)
            return NopDisposable.instance
        }

        let sink = SequenceSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}