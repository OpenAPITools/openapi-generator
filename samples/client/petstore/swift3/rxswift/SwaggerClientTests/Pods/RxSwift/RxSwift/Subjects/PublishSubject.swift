//
//  PublishSubject.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/11/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents an object that is both an observable sequence as well as an observer.

Each notification is broadcasted to all subscribed observers.
*/
final public class PublishSubject<Element>
    : Observable<Element>
    , SubjectType
    , Cancelable
    , ObserverType
    , SynchronizedUnsubscribeType {
    public typealias SubjectObserverType = PublishSubject<Element>
    
    typealias DisposeKey = Bag<AnyObserver<Element>>.KeyType
    
    /**
     Indicates whether the subject has any observers
     */
    public var hasObservers: Bool {
        _lock.lock(); defer { _lock.unlock() }
        return _observers.count > 0
    }
    
    private var _lock = NSRecursiveLock()
    
    // state
    private var _isDisposed = false
    private var _observers = Bag<AnyObserver<Element>>()
    private var _stopped = false
    private var _stoppedEvent = nil as Event<Element>?
    
    /**
    Indicates whether the subject has been isDisposed.
    */
    public var isDisposed: Bool {
        return _isDisposed
    }
    
    /**
    Creates a subject.
    */
    public override init() {
        super.init()
    }
    
    /**
    Notifies all subscribed observers about next event.
    
    - parameter event: Event to send to the observers.
    */
    public func on(_ event: Event<Element>) {
        _lock.lock(); defer { _lock.unlock() }
        _synchronized_on(event)
    }

    func _synchronized_on(_ event: Event<E>) {
        switch event {
        case .next(_):
            if _isDisposed || _stopped {
                return
            }
            
            _observers.on(event)
        case .completed, .error:
            if _stoppedEvent == nil {
                _stoppedEvent = event
                _stopped = true
                _observers.on(event)
                _observers.removeAll()
            }
        }
    }
    
    /**
    Subscribes an observer to the subject.
    
    - parameter observer: Observer to subscribe to the subject.
    - returns: Disposable object that can be used to unsubscribe the observer from the subject.
    */
    public override func subscribe<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        _lock.lock(); defer { _lock.unlock() }
        return _synchronized_subscribe(observer)
    }

    func _synchronized_subscribe<O : ObserverType>(_ observer: O) -> Disposable where O.E == E {
        if let stoppedEvent = _stoppedEvent {
            observer.on(stoppedEvent)
            return Disposables.create()
        }
        
        if _isDisposed {
            observer.on(.error(RxError.disposed(object: self)))
            return Disposables.create()
        }
        
        let key = _observers.insert(observer.asObserver())
        return SubscriptionDisposable(owner: self, key: key)
    }

    func synchronizedUnsubscribe(_ disposeKey: DisposeKey) {
        _lock.lock(); defer { _lock.unlock() }
        _synchronized_unsubscribe(disposeKey)
    }

    func _synchronized_unsubscribe(_ disposeKey: DisposeKey) {
        _ = _observers.removeKey(disposeKey)
    }
    
    /**
    Returns observer interface for subject.
    */
    public func asObserver() -> PublishSubject<Element> {
        return self
    }
    
    /**
    Unsubscribe all observers and release resources.
    */
    public func dispose() {
        _lock.lock(); defer { _lock.unlock() }
        _synchronized_dispose()
    }

    final func _synchronized_dispose() {
        _isDisposed = true
        _observers.removeAll()
        _stoppedEvent = nil
    }
}
