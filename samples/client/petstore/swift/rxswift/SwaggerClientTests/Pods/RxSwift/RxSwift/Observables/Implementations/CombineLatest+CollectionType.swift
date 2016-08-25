//
//  CombineLatest+CollectionType.swift
//  Rx
//
//  Created by Krunoslav Zaher on 8/29/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class CombineLatestCollectionTypeSink<C: CollectionType, R, O: ObserverType where C.Generator.Element : ObservableConvertibleType, O.E == R>
    : Sink<O> {
    typealias Parent = CombineLatestCollectionType<C, R>
    typealias SourceElement = C.Generator.Element.E

    let _parent: Parent

    let _lock = NSRecursiveLock()

    // state
    var _numberOfValues = 0
    var _values: [SourceElement?]
    var _isDone: [Bool]
    var _numberOfDone = 0
    var _subscriptions: [SingleAssignmentDisposable]

    init(parent: Parent, observer: O) {
        _parent = parent
        _values = [SourceElement?](count: parent._count, repeatedValue: nil)
        _isDone = [Bool](count: parent._count, repeatedValue: false)
        _subscriptions = Array<SingleAssignmentDisposable>()
        _subscriptions.reserveCapacity(parent._count)

        for _ in 0 ..< parent._count {
            _subscriptions.append(SingleAssignmentDisposable())
        }

        super.init(observer: observer)
    }

    func on(event: Event<SourceElement>, atIndex: Int) {
        _lock.lock(); defer { _lock.unlock() } // {
            switch event {
            case .Next(let element):
                if _values[atIndex] == nil {
                   _numberOfValues += 1
                }

                _values[atIndex] = element

                if _numberOfValues < _parent._count {
                    let numberOfOthersThatAreDone = self._numberOfDone - (_isDone[atIndex] ? 1 : 0)
                    if numberOfOthersThatAreDone == self._parent._count - 1 {
                        forwardOn(.Completed)
                        dispose()
                    }
                    return
                }

                do {
                    let result = try _parent._resultSelector(_values.map { $0! })
                    forwardOn(.Next(result))
                }
                catch let error {
                    forwardOn(.Error(error))
                    dispose()
                }

            case .Error(let error):
                forwardOn(.Error(error))
                dispose()
            case .Completed:
                if _isDone[atIndex] {
                    return
                }

                _isDone[atIndex] = true
                _numberOfDone += 1

                if _numberOfDone == self._parent._count {
                    forwardOn(.Completed)
                    dispose()
                }
                else {
                    _subscriptions[atIndex].dispose()
                }
            }
        // }
    }

    func run() -> Disposable {
        var j = 0
        for i in _parent._sources.startIndex ..< _parent._sources.endIndex {
            let index = j
            let source = _parent._sources[i].asObservable()
            _subscriptions[j].disposable = source.subscribe(AnyObserver { event in
                self.on(event, atIndex: index)
            })

            j += 1
        }

        return CompositeDisposable(disposables: _subscriptions.map { $0 })
    }
}

class CombineLatestCollectionType<C: CollectionType, R where C.Generator.Element : ObservableConvertibleType> : Producer<R> {
    typealias ResultSelector = [C.Generator.Element.E] throws -> R

    let _sources: C
    let _resultSelector: ResultSelector
    let _count: Int

    init(sources: C, resultSelector: ResultSelector) {
        _sources = sources
        _resultSelector = resultSelector
        _count = Int(self._sources.count.toIntMax())
    }

    override func run<O : ObserverType where O.E == R>(observer: O) -> Disposable {
        let sink = CombineLatestCollectionTypeSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}