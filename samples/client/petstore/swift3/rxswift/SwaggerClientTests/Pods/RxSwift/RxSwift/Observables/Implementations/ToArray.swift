//
//  ToArray.swift
//  Rx
//
//  Created by Junior B. on 20/10/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class ToArraySink<SourceType, O: ObserverType> : Sink<O>, ObserverType where O.E == [SourceType] {
    typealias Parent = ToArray<SourceType>
    
    let _parent: Parent
    var _list = Array<SourceType>()
    
    init(parent: Parent, observer: O) {
        _parent = parent
        
        super.init(observer: observer)
    }
    
    func on(_ event: Event<SourceType>) {
        switch event {
        case .next(let value):
            self._list.append(value)
        case .error(let e):
            forwardOn(.error(e))
            self.dispose()
        case .completed:
            forwardOn(.next(_list))
            forwardOn(.completed)
            self.dispose()
        }
    }
}

class ToArray<SourceType> : Producer<[SourceType]> {
    let _source: Observable<SourceType>

    init(source: Observable<SourceType>) {
        _source = source
    }
    
    override func run<O: ObserverType>(_ observer: O) -> Disposable where O.E == [SourceType] {
        let sink = ToArraySink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}
