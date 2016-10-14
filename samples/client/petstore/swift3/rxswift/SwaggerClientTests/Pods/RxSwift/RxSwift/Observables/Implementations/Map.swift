//
//  Map.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/15/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class MapSink<SourceType, O : ObserverType> : Sink<O>, ObserverType {
    typealias Selector = (SourceType) throws -> ResultType

    typealias ResultType = O.E
    typealias Element = SourceType

    private let _selector: Selector
    
    init(selector: @escaping Selector, observer: O) {
        _selector = selector
        super.init(observer: observer)
    }

    func on(_ event: Event<SourceType>) {
        switch event {
        case .next(let element):
            do {
                let mappedElement = try _selector(element)
                forwardOn(.next(mappedElement))
            }
            catch let e {
                forwardOn(.error(e))
                dispose()
            }
        case .error(let error):
            forwardOn(.error(error))
            dispose()
        case .completed:
            forwardOn(.completed)
            dispose()
        }
    }
}

class MapWithIndexSink<SourceType, O : ObserverType> : Sink<O>, ObserverType {
    typealias Selector = (SourceType, Int) throws -> ResultType

    typealias ResultType = O.E
    typealias Element = SourceType
    typealias Parent = MapWithIndex<SourceType, ResultType>
    
    private let _selector: Selector

    private var _index = 0

    init(selector: @escaping Selector, observer: O) {
        _selector = selector
        super.init(observer: observer)
    }

    func on(_ event: Event<SourceType>) {
        switch event {
        case .next(let element):
            do {
                let mappedElement = try _selector(element, try incrementChecked(&_index))
                forwardOn(.next(mappedElement))
            }
            catch let e {
                forwardOn(.error(e))
                dispose()
            }
        case .error(let error):
            forwardOn(.error(error))
            dispose()
        case .completed:
            forwardOn(.completed)
            dispose()
        }
    }
}

class MapWithIndex<SourceType, ResultType> : Producer<ResultType> {
    typealias Selector = (SourceType, Int) throws -> ResultType

    private let _source: Observable<SourceType>

    private let _selector: Selector

    init(source: Observable<SourceType>, selector: @escaping Selector) {
        _source = source
        _selector = selector
    }

    override func run<O: ObserverType>(_ observer: O) -> Disposable where O.E == ResultType {
        let sink = MapWithIndexSink(selector: _selector, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}

#if TRACE_RESOURCES
public var numberOfMapOperators: Int32 = 0
#endif

class Map<SourceType, ResultType>: Producer<ResultType> {
    typealias Selector = (SourceType) throws -> ResultType

    private let _source: Observable<SourceType>

    private let _selector: Selector

    init(source: Observable<SourceType>, selector: @escaping Selector) {
        _source = source
        _selector = selector

#if TRACE_RESOURCES
        let _ = AtomicIncrement(&numberOfMapOperators)
#endif
    }

    override func composeMap<R>(_ selector: @escaping (ResultType) throws -> R) -> Observable<R> {
        let originalSelector = _selector
        return Map<SourceType, R>(source: _source, selector: { (s: SourceType) throws -> R in
            let r: ResultType = try originalSelector(s)
            return try selector(r)
        })
    }
    
    override func run<O: ObserverType>(_ observer: O) -> Disposable where O.E == ResultType {
        let sink = MapSink(selector: _selector, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }

    #if TRACE_RESOURCES
    deinit {
        let _ = AtomicDecrement(&numberOfMapOperators)
    }
    #endif
}
