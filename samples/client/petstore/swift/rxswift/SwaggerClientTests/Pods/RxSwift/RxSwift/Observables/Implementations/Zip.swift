//
//  Zip.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 5/23/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

protocol ZipSinkProtocol : class
{
    func next(index: Int)
    func fail(error: ErrorType)
    func done(index: Int)
}

class ZipSink<O: ObserverType> : Sink<O>, ZipSinkProtocol {
    typealias Element = O.E

    let _arity: Int

    let _lock = NSRecursiveLock()

    // state
    private var _isDone: [Bool]

    init(arity: Int, observer: O) {
        _isDone = [Bool](count: arity, repeatedValue: false)
        _arity = arity

        super.init(observer: observer)
    }

    func getResult() throws -> Element {
        abstractMethod()
    }

    func hasElements(index: Int) -> Bool {
        abstractMethod()
    }

    func next(index: Int) {
        var hasValueAll = true

        for i in 0 ..< _arity {
            if !hasElements(i) {
                hasValueAll = false
                break
            }
        }

        if hasValueAll {
            do {
                let result = try getResult()
                self.forwardOn(.Next(result))
            }
            catch let e {
                self.forwardOn(.Error(e))
                dispose()
            }
        }
        else {
            var allOthersDone = true

            let arity = _isDone.count
            for i in 0 ..< arity {
                if i != index && !_isDone[i] {
                    allOthersDone = false
                    break
                }
            }

            if allOthersDone {
                forwardOn(.Completed)
                self.dispose()
            }
        }
    }

    func fail(error: ErrorType) {
        forwardOn(.Error(error))
        dispose()
    }

    func done(index: Int) {
        _isDone[index] = true

        var allDone = true

        for done in _isDone {
            if !done {
                allDone = false
                break
            }
        }

        if allDone {
            forwardOn(.Completed)
            dispose()
        }
    }
}

class ZipObserver<ElementType>
    : ObserverType
    , LockOwnerType
    , SynchronizedOnType {
    typealias E = ElementType
    typealias ValueSetter = (ElementType) -> ()

    private var _parent: ZipSinkProtocol?

    let _lock: NSRecursiveLock

    // state
    private let _index: Int
    private let _this: Disposable
    private let _setNextValue: ValueSetter

    init(lock: NSRecursiveLock, parent: ZipSinkProtocol, index: Int, setNextValue: ValueSetter, this: Disposable) {
        _lock = lock
        _parent = parent
        _index = index
        _this = this
        _setNextValue = setNextValue
    }

    func on(event: Event<E>) {
        synchronizedOn(event)
    }

    func _synchronized_on(event: Event<E>) {
        if let _ = _parent {
            switch event {
            case .Next(_):
                break
            case .Error(_):
                _this.dispose()
            case .Completed:
                _this.dispose()
            }
        }

        if let parent = _parent {
            switch event {
            case .Next(let value):
                _setNextValue(value)
                parent.next(_index)
            case .Error(let error):
                parent.fail(error)
            case .Completed:
                parent.done(_index)
            }
        }
    }
}
