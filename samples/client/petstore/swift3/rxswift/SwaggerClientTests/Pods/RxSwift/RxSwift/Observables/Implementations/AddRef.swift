//
//  AddRef.swift
//  Rx
//
//  Created by Junior B. on 30/10/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class AddRefSink<O: ObserverType> : Sink<O>, ObserverType {
    typealias Element = O.E
    
    override init(observer: O) {
        super.init(observer: observer)
    }
    
    func on(_ event: Event<Element>) {
        switch event {
        case .next(_):
            forwardOn(event)
        case .completed, .error(_):
            forwardOn(event)
            dispose()
        }
    }
}

class AddRef<Element> : Producer<Element> {
    typealias EventHandler = (Event<Element>) throws -> Void
    
    private let _source: Observable<Element>
    private let _refCount: RefCountDisposable
    
    init(source: Observable<Element>, refCount: RefCountDisposable) {
        _source = source
        _refCount = refCount
    }
    
    override func run<O: ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        let releaseDisposable = _refCount.retain()
        let sink = AddRefSink(observer: observer)
        sink.disposable = Disposables.create(releaseDisposable, _source.subscribeSafe(sink))

        return sink
    }
}
