//
//  Lock.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/31/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

protocol Lock {
    func lock()
    func unlock()
}

#if os(Linux)
  import Glibc

  /**
  Simple wrapper for spin lock.
  */
  class SpinLock {
      private var _lock: pthread_spinlock_t = 0

      init() {
          if (pthread_spin_init(&_lock, 0) != 0) {
              fatalError("Spin lock initialization failed")
          }
      }

      func lock() {
          pthread_spin_lock(&_lock)
      }

      func unlock() {
          pthread_spin_unlock(&_lock)
      }

      func performLocked(@noescape action: () -> Void) {
          lock(); defer { unlock() }
          action()
      }

      func calculateLocked<T>(@noescape action: () -> T) -> T {
          lock(); defer { unlock() }
          return action()
      }

      func calculateLockedOrFail<T>(@noescape action: () throws -> T) throws -> T {
          lock(); defer { unlock() }
          let result = try action()
          return result
      }

      deinit {
          pthread_spin_destroy(&_lock)
      }
  }
#else

    // https://lists.swift.org/pipermail/swift-dev/Week-of-Mon-20151214/000321.html
    typealias SpinLock = NSRecursiveLock
#endif

extension NSRecursiveLock : Lock {
    func performLocked(_ action: () -> Void) {
        lock(); defer { unlock() }
        action()
    }

    func calculateLocked<T>(_ action: () -> T) -> T {
        lock(); defer { unlock() }
        return action()
    }

    func calculateLockedOrFail<T>(_ action: () throws -> T) throws -> T {
        lock(); defer { unlock() }
        let result = try action()
        return result
    }
}

/*
let RECURSIVE_MUTEX = _initializeRecursiveMutex()

func _initializeRecursiveMutex() -> pthread_mutex_t {
    var mutex: pthread_mutex_t = pthread_mutex_t()
    var mta: pthread_mutexattr_t = pthread_mutexattr_t()

    pthread_mutex_init(&mutex, nil)
    pthread_mutexattr_init(&mta)
    pthread_mutexattr_settype(&mta, PTHREAD_MUTEX_RECURSIVE)
    pthread_mutex_init(&mutex, &mta)

    return mutex
}

extension pthread_mutex_t {
    mutating func lock() {
        pthread_mutex_lock(&self)
    }

    mutating func unlock() {
        pthread_mutex_unlock(&self)
    }
}
*/
