//
//  Using.swift
//  Rx
//
//  Created by Yury Korolev on 10/15/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class UsingSink<SourceType, ResourceType: Disposable, O: ObserverType where O.E == SourceType> : Sink<O>, ObserverType {

    typealias Parent = Using<SourceType, ResourceType>
    typealias E = O.E

    private let _parent: Parent

    init(parent: Parent, observer: O) {
        _parent = parent
        super.init(observer: observer)
    }

    func run() -> Disposable {
        var disposable = NopDisposable.instance

        do {
            let resource = try _parent._resourceFactory()
            disposable = resource
            let source = try _parent._observableFactory(resource)

            return StableCompositeDisposable.create(
                source.subscribe(self),
                disposable
            )
        } catch let error {
            return StableCompositeDisposable.create(
                Observable.error(error).subscribe(self),
                disposable
            )
        }
    }

    func on(event: Event<E>) {
        switch event {
        case let .Next(value):
            forwardOn(.Next(value))
        case let .Error(error):
            forwardOn(.Error(error))
            dispose()
        case .Completed:
            forwardOn(.Completed)
            dispose()
        }
    }
}

class Using<SourceType, ResourceType: Disposable>: Producer<SourceType> {

    typealias E = SourceType

    typealias ResourceFactory = () throws -> ResourceType
    typealias ObservableFactory = ResourceType throws -> Observable<SourceType>

    private let _resourceFactory: ResourceFactory
    private let _observableFactory: ObservableFactory


    init(resourceFactory: ResourceFactory, observableFactory: ObservableFactory) {
        _resourceFactory = resourceFactory
        _observableFactory = observableFactory
    }

    override func run<O : ObserverType where O.E == E>(observer: O) -> Disposable {
        let sink = UsingSink(parent: self, observer: observer)
        sink.disposable = sink.run()
        return sink
    }
}