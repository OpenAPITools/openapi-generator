//
//  Timer.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/7/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class TimerSink<O: ObserverType> : Sink<O> where O.E : SignedInteger  {
    typealias Parent = Timer<O.E>
    
    private let _parent: Parent
    
    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }
    
    func run() -> Disposable {
        return _parent._scheduler.schedulePeriodic(0 as O.E, startAfter: _parent._dueTime, period: _parent._period!) { state in
            self.forwardOn(.next(state))
            return state &+ 1
        }
    }
}

class TimerOneOffSink<O: ObserverType> : Sink<O> where O.E : SignedInteger {
    typealias Parent = Timer<O.E>
    
    private let _parent: Parent
    
    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }
    
    func run() -> Disposable {
        return _parent._scheduler.scheduleRelative((), dueTime: _parent._dueTime) { (_) -> Disposable in
            self.forwardOn(.next(0))
            self.forwardOn(.completed)
            
            return Disposables.create()
        }
    }
}

class Timer<E: SignedInteger>: Producer<E> {
    fileprivate let _scheduler: SchedulerType
    fileprivate let _dueTime: RxTimeInterval
    fileprivate let _period: RxTimeInterval?
    
    init(dueTime: RxTimeInterval, period: RxTimeInterval?, scheduler: SchedulerType) {
        _scheduler = scheduler
        _dueTime = dueTime
        _period = period
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == E {
        if let _ = _period {
            let sink = TimerSink(parent: self, observer: observer)
            sink.disposable = sink.run()
            return sink
        }
        else {
            let sink = TimerOneOffSink(parent: self, observer: observer)
            sink.disposable = sink.run()
            return sink
        }
    }
}
