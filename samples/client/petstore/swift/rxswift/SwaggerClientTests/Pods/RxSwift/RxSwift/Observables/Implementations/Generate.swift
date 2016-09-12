//
//  Generate.swift
//  Rx
//
//  Created by Krunoslav Zaher on 9/2/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class GenerateSink<S, O: ObserverType> : Sink<O> {
    typealias Parent = Generate<S, O.E>

    private let _parent: Parent

    private var _state: S

    init(parent: Parent, observer: O) {
        _parent = parent
        _state = parent._initialState
        super.init(observer: observer)
    }

    func run() -> Disposable {
        return _parent._scheduler.scheduleRecursive(true) { (isFirst, recurse) -> Void in
            do {
                if !isFirst {
                    self._state = try self._parent._iterate(self._state)
                }

                if try self._parent._condition(self._state) {
                    let result = try self._parent._resultSelector(self._state)
                    self.forwardOn(.Next(result))

                    recurse(false)
                }
                else {
                    self.forwardOn(.Completed)
                    self.dispose()
                }
            }
            catch let error {
                self.forwardOn(.Error(error))
                self.dispose()
            }
        }
    }
}

class Generate<S, E> : Producer<E> {
    private let _initialState: S
    private let _condition: S throws -> Bool
    private let _iterate: S throws -> S
    private let _resultSelector: S throws -> E
    private let _scheduler: ImmediateSchedulerType

    init(initialState: S, condition: S throws -> Bool, iterate: S throws -> S, resultSelector: S throws -> E, scheduler: ImmediateSchedulerType) {
        _initialState = initialState
        _condition = condition
        _iterate = iterate
        _resultSelector = resultSelector
        _scheduler = scheduler
        super.init()
    }

    override func run<O : ObserverType where O.E == E>(observer: O) -> Disposable {
        let sink = GenerateSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}