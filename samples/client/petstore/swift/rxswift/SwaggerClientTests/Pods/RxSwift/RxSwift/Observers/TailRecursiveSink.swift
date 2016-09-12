//
//  TailRecursiveSink.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/21/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

enum TailRecursiveSinkCommand {
    case MoveNext
    case Dispose
}

#if DEBUG || TRACE_RESOURCES
    public var maxTailRecursiveSinkStackSize = 0
#endif

/// This class is usually used with `Generator` version of the operators.
class TailRecursiveSink<S: SequenceType, O: ObserverType where S.Generator.Element: ObservableConvertibleType, S.Generator.Element.E == O.E>
    : Sink<O>
    , InvocableWithValueType {
    typealias Value = TailRecursiveSinkCommand
    typealias E = O.E
    typealias SequenceGenerator = (generator: S.Generator, remaining: IntMax?)

    var _generators: [SequenceGenerator] = []
    var _disposed = false
    var _subscription = SerialDisposable()

    // this is thread safe object
    var _gate = AsyncLock<InvocableScheduledItem<TailRecursiveSink<S, O>>>()

    override init(observer: O) {
        super.init(observer: observer)
    }

    func run(sources: SequenceGenerator) -> Disposable {
        _generators.append(sources)

        schedule(.MoveNext)

        return _subscription
    }

    func invoke(command: TailRecursiveSinkCommand) {
        switch command {
        case .Dispose:
            disposeCommand()
        case .MoveNext:
            moveNextCommand()
        }
    }

    // simple implementation for now
    func schedule(command: TailRecursiveSinkCommand) {
        _gate.invoke(InvocableScheduledItem(invocable: self, state: command))
    }

    func done() {
        forwardOn(.Completed)
        dispose()
    }

    func extract(observable: Observable<E>) -> SequenceGenerator? {
        abstractMethod()
    }

    // should be done on gate locked

    private func moveNextCommand() {
        var next: Observable<E>? = nil

        repeat {
            guard let (g, left) = _generators.last else {
                break
            }

            if _disposed {
                return
            }

            _generators.removeLast()

            var e = g

            guard let nextCandidate = e.next()?.asObservable() else {
                continue
            }

            // `left` is a hint of how many elements are left in generator.
            // In case this is the last element, then there is no need to push
            // that generator on stack.
            //
            // This is an optimization used to make sure in tail recursive case
            // there is no memory leak in case this operator is used to generate non terminating
            // sequence.

            if let knownOriginalLeft = left {
                // `- 1` because generator.next() has just been called
                if knownOriginalLeft - 1 >= 1 {
                    _generators.append((e, knownOriginalLeft - 1))
                }
            }
            else {
                _generators.append((e, nil))
            }

            let nextGenerator = extract(nextCandidate)

            if let nextGenerator = nextGenerator {
                _generators.append(nextGenerator)
                #if DEBUG || TRACE_RESOURCES
                    if maxTailRecursiveSinkStackSize < _generators.count {
                        maxTailRecursiveSinkStackSize = _generators.count
                    }
                #endif
            }
            else {
                next = nextCandidate
            }
        } while next == nil

        if next == nil  {
            done()
            return
        }

        let disposable = SingleAssignmentDisposable()
        _subscription.disposable = disposable
        disposable.disposable = subscribeToNext(next!)
    }

    func subscribeToNext(source: Observable<E>) -> Disposable {
        abstractMethod()
    }

    func disposeCommand() {
        _disposed = true
        _generators.removeAll(keepCapacity: false)
    }

    override func dispose() {
        super.dispose()

        _subscription.dispose()

        schedule(.Dispose)
    }
}
