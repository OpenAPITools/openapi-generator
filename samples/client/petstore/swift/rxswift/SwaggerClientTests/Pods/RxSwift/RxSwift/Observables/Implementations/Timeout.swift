//
//  Timeout.swift
//  Rx
//
//  Created by Tomi Koskinen on 13/11/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class TimeoutSink<ElementType, O: ObserverType where O.E == ElementType>: Sink<O>, LockOwnerType, ObserverType {
    typealias E = ElementType
    typealias Parent = Timeout<E>

    private let _parent: Parent

    let _lock = NSRecursiveLock()

    private let _timerD = SerialDisposable()
    private let _subscription = SerialDisposable()

    private var _id = 0
    private var _switched = false

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        let original = SingleAssignmentDisposable()
        _subscription.disposable = original

        _createTimeoutTimer()

        original.disposable = _parent._source.subscribeSafe(self)

        return StableCompositeDisposable.create(_subscription, _timerD)
    }

    func on(event: Event<E>) {
        switch event {
        case .Next:
            var onNextWins = false

            _lock.performLocked() {
                onNextWins = !self._switched
                if onNextWins {
                    self._id = self._id &+ 1
                }
            }

            if onNextWins {
                forwardOn(event)
                self._createTimeoutTimer()
            }
        case .Error, .Completed:
            var onEventWins = false

            _lock.performLocked() {
                onEventWins = !self._switched
                if onEventWins {
                    self._id = self._id &+ 1
                }
            }

            if onEventWins {
                forwardOn(event)
                self.dispose()
            }
        }
    }

    private func _createTimeoutTimer() {
        if _timerD.disposed {
            return
        }

        let nextTimer = SingleAssignmentDisposable()
        _timerD.disposable = nextTimer

        nextTimer.disposable = _parent._scheduler.scheduleRelative(_id, dueTime: _parent._dueTime) { state in

            var timerWins = false

            self._lock.performLocked() {
                self._switched = (state == self._id)
                timerWins = self._switched
            }

            if timerWins {
                self._subscription.disposable = self._parent._other.subscribeSafe(self.forwarder())
            }

            return NopDisposable.instance
        }
    }
}


class Timeout<Element> : Producer<Element> {

    private let _source: Observable<Element>
    private let _dueTime: RxTimeInterval
    private let _other: Observable<Element>
    private let _scheduler: SchedulerType

    init(source: Observable<Element>, dueTime: RxTimeInterval, other: Observable<Element>, scheduler: SchedulerType) {
        _source = source
        _dueTime = dueTime
        _other = other
        _scheduler = scheduler
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = TimeoutSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}
