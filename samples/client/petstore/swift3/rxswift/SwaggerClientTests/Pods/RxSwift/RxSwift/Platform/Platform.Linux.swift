//
//  Platform.Linux.swift
//  Rx
//
//  Created by Krunoslav Zaher on 12/29/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

#if os(Linux)
    ////////////////////////////////////////////////////////////////////////////////
    // This is not the greatest API in the world, this is just a tribute.
    // !!! Proof of concept until libdispatch becomes operational. !!!
    ////////////////////////////////////////////////////////////////////////////////

    import Foundation
    import XCTest
    import Glibc
    import SwiftShims

    // MARK: CoreFoundation run loop mock

    public typealias CFRunLoopRef = Int
    public let kCFRunLoopDefaultMode = "CFRunLoopDefaultMode"

    typealias Action = () -> ()

    var queue = Queue<Action>(capacity: 100)

    var runLoopCounter = 0

    extension NSThread {
        public var isMainThread: Bool {
            return true
        }
    }

    public func CFRunLoopWakeUp(_ runLoop: CFRunLoopRef) {
    }

    public func CFRunLoopStop(_ runLoop: CFRunLoopRef) {
        runLoopCounter -= 1
    }

    public func CFRunLoopPerformBlock(_ runLoop: CFRunLoopRef, _ mode: String, _ action: () -> ()) {
        queue.enqueue(element: action)
    }

    public func CFRunLoopRun() {
        runLoopCounter += 1
        let currentValueOfCounter = runLoopCounter
        while let front = queue.dequeue() {
            front()
            if runLoopCounter < currentValueOfCounter - 1 {
                fatalError("called stop twice")
            }

            if runLoopCounter == currentValueOfCounter - 1 {
                break
            }
        }
    }

    public func CFRunLoopGetCurrent() -> CFRunLoopRef {
        return 0
    }

    // MARK: Atomic, just something that works for single thread case

    #if TRACE_RESOURCES
    public typealias AtomicInt = Int64
    #else
    typealias AtomicInt = Int64
    #endif

    func AtomicIncrement(_ increment: UnsafeMutablePointer<AtomicInt>) -> AtomicInt {
        increment.memory = increment.memory + 1
        return increment.memory
    }

    func AtomicDecrement(_ increment: UnsafeMutablePointer<AtomicInt>) -> AtomicInt {
        increment.memory = increment.memory - 1
        return increment.memory
    }

    func AtomicCompareAndSwap(_ l: AtomicInt, _ r: AtomicInt, _ target: UnsafeMutablePointer<AtomicInt>) -> Bool {
        //return __sync_val_compare_and_swap(target, l, r)
        if target.memory == l {
            target.memory = r
            return true
        }

        return false
    }

    extension NSThread {
        static func setThreadLocalStorageValue<T: AnyObject>(value: T?, forKey key: String) {
            let currentThread = NSThread.currentThread()
            var threadDictionary = currentThread.threadDictionary

            if let newValue = value {
                threadDictionary[key] = newValue
            }
            else {
                threadDictionary[key] = nil
            }

            currentThread.threadDictionary = threadDictionary
        }

        static func getThreadLocalStorageValueForKey<T: AnyObject>(key: String) -> T? {
            let currentThread = NSThread.currentThread()
            let threadDictionary = currentThread.threadDictionary

            return threadDictionary[key] as? T
        }
    }

    //

    // MARK: objc mock

    public func objc_sync_enter(_ lock: AnyObject) {
    }

    public func objc_sync_exit(_ lock: AnyObject) {

    }


    // MARK: libdispatch

    public typealias dispatch_time_t = Int
    public typealias dispatch_source_t = Int
    public typealias dispatch_source_type_t = Int
    public typealias dispatch_queue_t = Int
    public typealias dispatch_object_t = Int
    public typealias dispatch_block_t = () -> ()
    public typealias dispatch_queue_attr_t = Int
    public typealias qos_class_t = Int

    public let DISPATCH_QUEUE_SERIAL = 0

    public let DISPATCH_QUEUE_PRIORITY_HIGH = 1
    public let DISPATCH_QUEUE_PRIORITY_DEFAULT = 2
    public let DISPATCH_QUEUE_PRIORITY_LOW = 3

    public let QOS_CLASS_USER_INTERACTIVE = 0
    public let QOS_CLASS_USER_INITIATED = 1
    public let QOS_CLASS_DEFAULT = 2
    public let QOS_CLASS_UTILITY = 3
    public let QOS_CLASS_BACKGROUND = 4

    public let DISPATCH_SOURCE_TYPE_TIMER = 0
    public let DISPATCH_TIME_FOREVER = 1 as UInt64
    public let NSEC_PER_SEC = 1

    public let DISPATCH_TIME_NOW = -1

    public func dispatch_time(_ when: dispatch_time_t, _ delta: Int64) -> dispatch_time_t {
        return when + Int(delta)
    }

    public func dispatch_queue_create(_ label: UnsafePointer<Int8>, _ attr: dispatch_queue_attr_t!) -> dispatch_queue_t! {
        return 0
    }

    public func dispatch_set_target_queue(_ object: dispatch_object_t!, _ queue: dispatch_queue_t!) {
    }

    public func dispatch_async(_ queue2: dispatch_queue_t, _ block: dispatch_block_t) {
        queue.enqueue(block)
    }

    public func dispatch_source_create(_ type: dispatch_source_type_t, _ handle: UInt, _ mask: UInt, _ queue: dispatch_queue_t!) -> dispatch_source_t! {
        return 0
    }

    public func dispatch_source_set_timer(_ source: dispatch_source_t, _ start: dispatch_time_t, _ interval: UInt64, _ leeway: UInt64) {

    }

    public func dispatch_source_set_event_handler(_ source: dispatch_source_t, _ handler: dispatch_block_t!) {
        queue.enqueue(handler)
    }

    public func dispatch_resume(_ object: dispatch_object_t) {
    }

    public func dispatch_source_cancel(_ source: dispatch_source_t) {
    }

    public func dispatch_get_global_queue(_ identifier: Int, _ flags: UInt) -> dispatch_queue_t! {
        return 0
    }

    public func dispatch_get_main_queue() -> dispatch_queue_t! {
        return 0
    }

    // MARK: XCTest

    public class Expectation {
        public func fulfill() {
        }
    }

    extension XCTestCase {
        public func setUp() {
        }

        public func tearDown() {
        }

        public func expectationWithDescription(description: String) -> Expectation {
            return Expectation()
        }

        public func waitForExpectationsWithTimeout(time: NSTimeInterval, action: Swift.Error? -> Void) {
        }
    }

#endif
