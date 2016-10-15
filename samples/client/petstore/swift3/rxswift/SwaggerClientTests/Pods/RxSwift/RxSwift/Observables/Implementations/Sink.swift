//
//  Sink.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/19/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class Sink<O : ObserverType> : SingleAssignmentDisposable {
    fileprivate let _observer: O

    init(observer: O) {
#if TRACE_RESOURCES
        let _ = AtomicIncrement(&resourceCount)
#endif
        _observer = observer
    }
    
    final func forwardOn(_ event: Event<O.E>) {
        if isDisposed {
            return
        }
        _observer.on(event)
    }
    
    final func forwarder() -> SinkForward<O> {
        return SinkForward(forward: self)
    }

    deinit {
#if TRACE_RESOURCES
       let _ =  AtomicDecrement(&resourceCount)
#endif
    }
}

class SinkForward<O: ObserverType>: ObserverType {
    typealias E = O.E
    
    private let _forward: Sink<O>
    
    init(forward: Sink<O>) {
        _forward = forward
    }
    
    func on(_ event: Event<E>) {
        switch event {
        case .next:
            _forward._observer.on(event)
        case .error, .completed:
            _forward._observer.on(event)
            _forward.dispose()
        }
    }
}
