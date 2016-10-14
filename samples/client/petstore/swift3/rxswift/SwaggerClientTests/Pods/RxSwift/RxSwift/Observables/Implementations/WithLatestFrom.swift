//
//  WithLatestFrom.swift
//  RxExample
//
//  Created by Yury Korolev on 10/19/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class WithLatestFromSink<FirstType, SecondType, ResultType, O: ObserverType>
    : Sink<O>
    , ObserverType
    , LockOwnerType
    , SynchronizedOnType where O.E == ResultType {

    typealias Parent = WithLatestFrom<FirstType, SecondType, ResultType>
    typealias E = FirstType
    
    fileprivate let _parent: Parent
    
    var _lock = NSRecursiveLock()
    fileprivate var _latest: SecondType?

    init(parent: Parent, observer: O) {
        _parent = parent
        
        super.init(observer: observer)
    }
    
    func run() -> Disposable {
        let sndSubscription = SingleAssignmentDisposable()
        let sndO = WithLatestFromSecond(parent: self, disposable: sndSubscription)
        
        sndSubscription.disposable = _parent._second.subscribe(sndO)
        let fstSubscription = _parent._first.subscribe(self)
        
        return Disposables.create(fstSubscription, sndSubscription)
    }

    func on(_ event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(_ event: Event<E>) {
        switch event {
        case let .next(value):
            guard let latest = _latest else { return }
            do {
                let res = try _parent._resultSelector(value, latest)
                
                forwardOn(.next(res))
            } catch let e {
                forwardOn(.error(e))
                dispose()
            }
        case .completed:
            forwardOn(.completed)
            dispose()
        case let .error(error):
            forwardOn(.error(error))
            dispose()
        }
    }
}

class WithLatestFromSecond<FirstType, SecondType, ResultType, O: ObserverType>
    : ObserverType
    , LockOwnerType
    , SynchronizedOnType where O.E == ResultType {
    
    typealias Parent = WithLatestFromSink<FirstType, SecondType, ResultType, O>
    typealias E = SecondType
    
    private let _parent: Parent
    private let _disposable: Disposable

    var _lock: NSRecursiveLock {
        return _parent._lock
    }

    init(parent: Parent, disposable: Disposable) {
        _parent = parent
        _disposable = disposable
    }
    
    func on(_ event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(_ event: Event<E>) {
        switch event {
        case let .next(value):
            _parent._latest = value
        case .completed:
            _disposable.dispose()
        case let .error(error):
            _parent.forwardOn(.error(error))
            _parent.dispose()
        }
    }
}

class WithLatestFrom<FirstType, SecondType, ResultType>: Producer<ResultType> {
    typealias ResultSelector = (FirstType, SecondType) throws -> ResultType
    
    fileprivate let _first: Observable<FirstType>
    fileprivate let _second: Observable<SecondType>
    fileprivate let _resultSelector: ResultSelector

    init(first: Observable<FirstType>, second: Observable<SecondType>, resultSelector: @escaping ResultSelector) {
        _first = first
        _second = second
        _resultSelector = resultSelector
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == ResultType {
        let sink = WithLatestFromSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
