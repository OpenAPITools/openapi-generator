//
//  ToArray.swift
//  Rx
//
//  Created by Junior B. on 20/10/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class ToArraySink<SourceType, O: ObserverType where O.E == [SourceType]> : Sink<O>, ObserverType {
    typealias Parent = ToArray<SourceType>

    let _parent: Parent
    var _list = Array<SourceType>()

    init(parent: Parent, observer: O) {
        _parent = parent

        super.init(observer: observer)
    }

    func on(event: Event<SourceType>) {
        switch event {
        case .Next(let value):
            self._list.append(value)
        case .Error(let e):
            forwardOn(.Error(e))
            self.dispose()
        case .Completed:
            forwardOn(.Next(_list))
            forwardOn(.Completed)
            self.dispose()
        }
    }
}

class ToArray<SourceType> : Producer<[SourceType]> {
    let _source: Observable<SourceType>

    init(source: Observable<SourceType>) {
        _source = source
    }

    override func run<O: ObserverType where O.E == [SourceType]>(observer: O) -> Disposable {
        let sink = ToArraySink(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}