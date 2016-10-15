//
//  Event.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/8/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents a sequence event.

Sequence grammar:
next\* (error | completed)
*/
public enum Event<Element> {
    /// Next element is produced.
    case next(Element)

    /// Sequence terminated with an error.
    case error(Swift.Error)

    /// Sequence completed successfully.
    case completed
}

extension Event : CustomDebugStringConvertible {
    /// - returns: Description of event.
    public var debugDescription: String {
        switch self {
        case .next(let value):
            return "next(\(value))"
        case .error(let error):
            return "error(\(error))"
        case .completed:
            return "completed"
        }
    }
}

extension Event {
    /// - returns: Is `Completed` or `Error` event.
    public var isStopEvent: Bool {
        switch self {
        case .next: return false
        case .error, .completed: return true
        }
    }

    /// - returns: If `Next` event, returns element value.
    public var element: Element? {
        if case .next(let value) = self {
            return value
        }
        return nil
    }

    /// - returns: If `Error` event, returns error.
    public var error: Swift.Error? {
        if case .error(let error) = self {
            return error
        }
        return nil
    }
}
