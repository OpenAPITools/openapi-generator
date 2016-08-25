//
//  Take.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/12/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

// count version

class TakeCountSink<ElementType, O: ObserverType where O.E == ElementType> : Sink<O>, ObserverType {
    typealias Parent = TakeCount<ElementType>
    typealias E = ElementType

    private let _parent: Parent

    private var _remaining: Int

    init(parent: Parent, observer: O) {
        _parent = parent
        _remaining = parent._count
        super.init(observer: observer)
    }

    func on(event: Event<E>) {
        switch event {
        case .Next(let value):

            if _remaining > 0 {
                _remaining -= 1

                forwardOn(.Next(value))

                if _remaining == 0 {
                    forwardOn(.Completed)
                    dispose()
                }
            }
        case .Error:
            forwardOn(event)
            dispose()
        case .Completed:
            forwardOn(event)
            dispose()
        }
    }

}

class TakeCount<Element>: Producer<Element> {
    private let _source: Observable<Element>
    private let _count: Int

    init(source: Observable<Element>, count: Int) {
        if count < 0 {
            rxFatalError("count can't be negative")
        }
        _source = source
        _count = count
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = TakeCountSink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}

// time version

class TakeTimeSink<ElementType, O: ObserverType where O.E == ElementType>
    : Sink<O>
    , LockOwnerType
    , ObserverType
    , SynchronizedOnType {
    typealias Parent = TakeTime<ElementType>
    typealias E = ElementType

    private let _parent: Parent

    let _lock = NSRecursiveLock()

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func on(event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(event: Event<E>) {
        switch event {
        case .Next(let value):
            forwardOn(.Next(value))
        case .Error:
            forwardOn(event)
            dispose()
        case .Completed:
            forwardOn(event)
            dispose()
        }
    }

    func tick() {
        _lock.lock(); defer { _lock.unlock() }

        forwardOn(.Completed)
        dispose()
    }

    func run() -> Disposable {
        let disposeTimer = _parent._scheduler.scheduleRelative((), dueTime: _parent._duration) {
            self.tick()
            return NopDisposable.instance
        }

        let disposeSubscription = _parent._source.subscribe(self)

        return BinaryDisposable(disposeTimer, disposeSubscription)
    }
}

class TakeTime<Element> : Producer<Element> {
    typealias TimeInterval = RxTimeInterval

    private let _source: Observable<Element>
    private let _duration: TimeInterval
    private let _scheduler: SchedulerType

    init(source: Observable<Element>, duration: TimeInterval, scheduler: SchedulerType) {
        _source = source
        _scheduler = scheduler
        _duration = duration
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = TakeTimeSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}