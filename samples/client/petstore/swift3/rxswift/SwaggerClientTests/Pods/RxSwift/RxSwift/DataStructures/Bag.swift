//
//  Bag.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/28/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation
import Swift

let arrayDictionaryMaxSize = 30

/**
Class that enables using memory allocations as a means to uniquely identify objects.
*/
class Identity {
    // weird things have known to happen with Swift
    var _forceAllocation: Int32 = 0
}

func hash(_ _x: Int) -> Int {
    var x = _x
    x = ((x >> 16) ^ x) &* 0x45d9f3b
    x = ((x >> 16) ^ x) &* 0x45d9f3b
    x = ((x >> 16) ^ x)
    return x;
}

/**
Unique identifier for object added to `Bag`.
*/
public struct BagKey : Hashable {
    let uniqueIdentity: Identity?
    let key: Int

    public var hashValue: Int {
        if let uniqueIdentity = uniqueIdentity {
            return hash(key) ^ (ObjectIdentifier(uniqueIdentity).hashValue)
        }
        else {
            return hash(key)
        }
    }
}

/**
Compares two `BagKey`s.
*/
public func == (lhs: BagKey, rhs: BagKey) -> Bool {
    return lhs.key == rhs.key && lhs.uniqueIdentity === rhs.uniqueIdentity
}

/**
Data structure that represents a bag of elements typed `T`.

Single element can be stored multiple times.

Time and space complexity of insertion an deletion is O(n). 

It is suitable for storing small number of elements.
*/
public struct Bag<T> : CustomDebugStringConvertible {
    /**
    Type of identifier for inserted elements.
    */
    public typealias KeyType = BagKey
    
    fileprivate typealias ScopeUniqueTokenType = Int
    
    typealias Entry = (key: BagKey, value: T)
 
    fileprivate var _uniqueIdentity: Identity?
    fileprivate var _nextKey: ScopeUniqueTokenType = 0

    // data

    // first fill inline variables
    fileprivate var _key0: BagKey? = nil
    fileprivate var _value0: T? = nil

    fileprivate var _key1: BagKey? = nil
    fileprivate var _value1: T? = nil

    // then fill "array dictionary"
    fileprivate var _pairs = ContiguousArray<Entry>()

    // last is sparse dictionary
    fileprivate var _dictionary: [BagKey : T]? = nil

    fileprivate var _onlyFastPath = true

    /**
    Creates new empty `Bag`.
    */
    public init() {
    }
    
    /**
    Inserts `value` into bag.
    
    - parameter element: Element to insert.
    - returns: Key that can be used to remove element from bag.
    */
    public mutating func insert(_ element: T) -> BagKey {
        _nextKey = _nextKey &+ 1

#if DEBUG
        _nextKey = _nextKey % 20
#endif
        
        if _nextKey == 0 {
            _uniqueIdentity = Identity()
        }

        let key = BagKey(uniqueIdentity: _uniqueIdentity, key: _nextKey)

        if _key0 == nil {
            _key0 = key
            _value0 = element
            return key
        }

        _onlyFastPath = false

        if _key1 == nil {
            _key1 = key
            _value1 = element
            return key
        }

        if _dictionary != nil {
            _dictionary![key] = element
            return key
        }

        if _pairs.count < arrayDictionaryMaxSize {
            _pairs.append(key: key, value: element)
            return key
        }

        if _dictionary == nil {
            _dictionary = [:]
        }

        _dictionary![key] = element
        
        return key
    }
    
    /**
    - returns: Number of elements in bag.
    */
    public var count: Int {
        let dictionaryCount: Int = _dictionary?.count ?? 0
        return _pairs.count + (_value0 != nil ? 1 : 0) + (_value1 != nil ? 1 : 0) + dictionaryCount
    }
    
    /**
    Removes all elements from bag and clears capacity.
    */
    public mutating func removeAll() {
        _key0 = nil
        _value0 = nil
        _key1 = nil
        _value1 = nil

        _pairs.removeAll(keepingCapacity: false)
        _dictionary?.removeAll(keepingCapacity: false)
    }
    
    /**
    Removes element with a specific `key` from bag.
    
    - parameter key: Key that identifies element to remove from bag.
    - returns: Element that bag contained, or nil in case element was already removed.
    */
    public mutating func removeKey(_ key: BagKey) -> T? {
        if _key0 == key {
            _key0 = nil
            let value = _value0!
            _value0 = nil
            return value
        }

        if _key1 == key {
            _key1 = nil
            let value = _value1!
            _value1 = nil
            return value
        }

        if let existingObject = _dictionary?.removeValue(forKey: key) {
            return existingObject
        }

        for i in 0 ..< _pairs.count {
            if _pairs[i].key == key {
                let value = _pairs[i].value
                _pairs.remove(at: i)
                return value
            }
        }

        return nil
    }
}

extension Bag {
    /**
    A textual representation of `self`, suitable for debugging.
    */
    public var debugDescription : String {
        return "\(self.count) elements in Bag"
    }
}


// MARK: forEach

extension Bag {
    /**
    Enumerates elements inside the bag.
    
    - parameter action: Enumeration closure.
    */
    public func forEach(_ action: (T) -> Void) {
        if _onlyFastPath {
            if let value0 = _value0 {
                action(value0)
            }
            return
        }

        let pairs = _pairs
        let value0 = _value0
        let value1 = _value1
        let dictionary = _dictionary

        if let value0 = value0 {
            action(value0)
        }

        if let value1 = value1 {
            action(value1)
        }

        for i in 0 ..< pairs.count {
            action(pairs[i].value)
        }

        if dictionary?.count ?? 0 > 0 {
            for element in dictionary!.values {
                action(element)
            }
        }
    }
}

extension Bag where T: ObserverType {
    /**
     Dispatches `event` to app observers contained inside bag.

     - parameter action: Enumeration closure.
     */
    public func on(_ event: Event<T.E>) {
        if _onlyFastPath {
            _value0?.on(event)
            return
        }

        let pairs = _pairs
        let value0 = _value0
        let value1 = _value1
        let dictionary = _dictionary

        if let value0 = value0 {
            value0.on(event)
        }

        if let value1 = value1 {
            value1.on(event)
        }

        for i in 0 ..< pairs.count {
            pairs[i].value.on(event)
        }

        if dictionary?.count ?? 0 > 0 {
            for element in dictionary!.values {
                element.on(event)
            }
        }
    }
}

/**
Dispatches `dispose` to all disposables contained inside bag.
*/
@available(*, deprecated, renamed: "disposeAll(in:)")
public func disposeAllIn(_ bag: Bag<Disposable>) {
    disposeAll(in: bag)
}

/**
 Dispatches `dispose` to all disposables contained inside bag.
 */
public func disposeAll(in bag: Bag<Disposable>) {
    if bag._onlyFastPath {
        bag._value0?.dispose()
        return
    }

    let pairs = bag._pairs
    let value0 = bag._value0
    let value1 = bag._value1
    let dictionary = bag._dictionary

    if let value0 = value0 {
        value0.dispose()
    }

    if let value1 = value1 {
        value1.dispose()
    }

    for i in 0 ..< pairs.count {
        pairs[i].value.dispose()
    }

    if dictionary?.count ?? 0 > 0 {
        for element in dictionary!.values {
            element.dispose()
        }
    }
}
