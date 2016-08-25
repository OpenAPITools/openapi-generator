//
//  Platform.Darwin.swift
//  Rx
//
//  Created by Krunoslav Zaher on 12/29/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

#if os(OSX) || os(iOS) || os(tvOS) || os(watchOS)

    import Darwin
    import Foundation

    #if TRACE_RESOURCES
    public typealias AtomicInt = Int32
    #else
    typealias AtomicInt = Int32
    #endif

    let AtomicCompareAndSwap = OSAtomicCompareAndSwap32
    let AtomicIncrement = OSAtomicIncrement32
    let AtomicDecrement = OSAtomicDecrement32

    extension NSThread {
        static func setThreadLocalStorageValue<T: AnyObject>(value: T?, forKey key: protocol<AnyObject, NSCopying>) {
            let currentThread = NSThread.currentThread()
            let threadDictionary = currentThread.threadDictionary

            if let newValue = value {
                threadDictionary.setObject(newValue, forKey: key)
            }
            else {
                threadDictionary.removeObjectForKey(key)
            }

        }
        static func getThreadLocalStorageValueForKey<T>(key: protocol<AnyObject, NSCopying>) -> T? {
            let currentThread = NSThread.currentThread()
            let threadDictionary = currentThread.threadDictionary

            return threadDictionary[key] as? T
        }
    }

#endif
