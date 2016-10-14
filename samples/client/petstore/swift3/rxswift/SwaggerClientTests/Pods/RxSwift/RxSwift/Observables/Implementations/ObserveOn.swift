//
//  ObserveOn.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 7/25/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class ObserveOn<E> : Producer<E> {
    let scheduler: ImmediateSchedulerType
    let source: Observable<E>
    
    init(source: Observable<E>, scheduler: ImmediateSchedulerType) {
        self.scheduler = scheduler
        self.source = source
        
#if TRACE_RESOURCES
        let _ = AtomicIncrement(&resourceCount)
#endif
    }
    
    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == E {
        let sink = ObserveOnSink(scheduler: scheduler, observer: observer)
        sink._subscription.disposable = source.subscribe(sink)
        return sink
    }
    
#if TRACE_RESOURCES
    deinit {
        let _ = AtomicDecrement(&resourceCount)
    }
#endif
}

enum ObserveOnState : Int32 {
    // pump is not running
    case stopped = 0
    // pump is running
    case running = 1
}

class ObserveOnSink<O: ObserverType> : ObserverBase<O.E> {
    typealias E = O.E
    
    let _scheduler: ImmediateSchedulerType

    var _lock = SpinLock()

    // state
    var _state = ObserveOnState.stopped
    var _observer: O?
    var _queue = Queue<Event<E>>(capacity: 10)

    let _scheduleDisposable = SerialDisposable()
    let _subscription = SingleAssignmentDisposable()

    init(scheduler: ImmediateSchedulerType, observer: O) {
        _scheduler = scheduler
        _observer = observer
    }

    override func onCore(_ event: Event<E>) {
        let shouldStart = _lock.calculateLocked { () -> Bool in
            self._queue.enqueue(event)
            
            switch self._state {
            case .stopped:
                self._state = .running
                return true
            case .running:
                return false
            }
        }
        
        if shouldStart {
            _scheduleDisposable.disposable = self._scheduler.scheduleRecursive((), action: self.run)
        }
    }
    
    func run(_ state: Void, recurse: (Void) -> Void) {
        let (nextEvent, observer) = self._lock.calculateLocked { () -> (Event<E>?, O?) in
            if self._queue.count > 0 {
                return (self._queue.dequeue(), self._observer)
            }
            else {
                self._state = .stopped
                return (nil, self._observer)
            }
        }
        
        if let nextEvent = nextEvent {
            observer?.on(nextEvent)
            if nextEvent.isStopEvent {
                dispose()
            }
        }
        else {
            return
        }
        
        let shouldContinue = _shouldContinue_synchronized()
        
        if shouldContinue {
            recurse()
        }
    }

    func _shouldContinue_synchronized() -> Bool {
        _lock.lock(); defer { _lock.unlock() } // {
            if self._queue.count > 0 {
                return true
            }
            else {
                self._state = .stopped
                return false
            }
        // }
    }
    
    override func dispose() {
        super.dispose()

        _subscription.dispose()
        _scheduleDisposable.dispose()

        _lock.lock(); defer { _lock.unlock() } // {
            _observer = nil
        
        // }
    }
}
