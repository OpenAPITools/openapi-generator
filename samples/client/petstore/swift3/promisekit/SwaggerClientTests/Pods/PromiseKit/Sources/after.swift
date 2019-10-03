import struct Foundation.TimeInterval
import Dispatch

/**
 - Returns: A new promise that fulfills after the specified duration.
*/
@available(*, deprecated: 4.3, message: "Use after(seconds:)")
public func after(interval: TimeInterval) -> Promise<Void> {
    return after(seconds: interval)
}

/**
     after(.seconds(2)).then {
     }

- Returns: A new promise that fulfills after the specified duration.
*/
public func after(seconds: TimeInterval) -> Promise<Void> {
    return Promise { fulfill, _ in
        let when = DispatchTime.now() + seconds
        DispatchQueue.global().asyncAfter(deadline: when) { fulfill(()) }
    }
}

/**
 - Returns: A new promise that fulfills after the specified duration.
*/
public func after(interval: DispatchTimeInterval) -> Promise<Void> {
    return Promise { fulfill, _ in
        let when = DispatchTime.now() + interval
    #if swift(>=4.0)
        DispatchQueue.global().asyncAfter(deadline: when) { fulfill(()) }
    #else
        DispatchQueue.global().asyncAfter(deadline: when, execute: fulfill)
    #endif
    }
}
