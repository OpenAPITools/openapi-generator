//
//  ShareReplay1.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 10/10/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

extension ObservableType {

    /**
     Returns an observable sequence that shares a single subscription to the underlying sequence, and immediately upon subscription replays maximum number of elements in buffer.

     This operator is a specialization of replay which creates a subscription when the number of observers goes from zero to one, then shares that subscription with all subsequent observers until the number of observers returns to zero, at which point the subscription is disposed.

     - seealso: [shareReplay operator on reactivex.io](http://reactivex.io/documentation/operators/replay.html)

     - parameter bufferSize: Maximum element count of the replay buffer.
     - returns: An observable sequence that contains the elements of a sequence produced by multicasting the source sequence.
     */
    public func shareReplay(_ bufferSize: Int)
        -> Observable<E> {
            if bufferSize == 1 {
                return ShareReplay1(source: self.asObservable())
            }
            else {
                return self.replay(bufferSize).refCount()
            }
    }
}

// optimized version of share replay for most common case
final fileprivate class ShareReplay1<Element>
    : Observable<Element>
    , ObserverType
    , SynchronizedUnsubscribeType {

    typealias Observers = AnyObserver<Element>.s
    typealias DisposeKey = Observers.KeyType

    private let _source: Observable<Element>

    private let _lock = RecursiveLock()

    private var _connection: SingleAssignmentDisposable?
    private var _element: Element?
    private var _stopped = false
    private var _stopEvent = nil as Event<Element>?
    private var _observers = Observers()

    init(source: Observable<Element>) {
        self._source = source
    }

    override func subscribe<O : ObserverType>(_ observer: O) -> Disposable where O.E == E {
        _lock.lock()
        let result = _synchronized_subscribe(observer)
        _lock.unlock()
        return result
    }

    func _synchronized_subscribe<O : ObserverType>(_ observer: O) -> Disposable where O.E == E {
        if let element = self._element {
            observer.on(.next(element))
        }

        if let stopEvent = self._stopEvent {
            observer.on(stopEvent)
            return Disposables.create()
        }

        let initialCount = self._observers.count

        let disposeKey = self._observers.insert(observer.on)

        if initialCount == 0 {
            let connection = SingleAssignmentDisposable()
            _connection = connection

            connection.setDisposable(self._source.subscribe(self))
        }

        return SubscriptionDisposable(owner: self, key: disposeKey)
    }

    func synchronizedUnsubscribe(_ disposeKey: DisposeKey) {
        _lock.lock()
        _synchronized_unsubscribe(disposeKey)
        _lock.unlock()
    }

    func _synchronized_unsubscribe(_ disposeKey: DisposeKey) {
        // if already unsubscribed, just return
        if self._observers.removeKey(disposeKey) == nil {
            return
        }

        if _observers.count == 0 {
            _connection?.dispose()
            _connection = nil
        }
    }

    func on(_ event: Event<E>) {
        dispatch(_synchronized_on(event), event)
    }

    func _synchronized_on(_ event: Event<E>) -> Observers {
        _lock.lock(); defer { _lock.unlock() }
        if _stopped {
            return Observers()
        }

        switch event {
        case .next(let element):
            _element = element
        case .error, .completed:
            _stopEvent = event
            _stopped = true
            _connection?.dispose()
            _connection = nil
        }
        
        return _observers
    }
    
}
