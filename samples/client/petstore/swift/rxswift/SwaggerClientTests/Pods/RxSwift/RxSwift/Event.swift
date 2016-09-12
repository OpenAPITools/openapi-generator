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
Next\* (Error | Completed)
*/
public enum Event<Element> {
    /// Next element is produced.
    case Next(Element)

    /// Sequence terminated with an error.
    case Error(ErrorType)

    /// Sequence completed successfully.
    case Completed
}

extension Event : CustomDebugStringConvertible {
    /// - returns: Description of event.
    public var debugDescription: String {
        switch self {
        case .Next(let value):
            return "Next(\(value))"
        case .Error(let error):
            return "Error(\(error))"
        case .Completed:
            return "Completed"
        }
    }
}

extension Event {
    /// - returns: Is `Completed` or `Error` event.
    public var isStopEvent: Bool {
        switch self {
        case .Next: return false
        case .Error, .Completed: return true
        }
    }

    /// - returns: If `Next` event, returns element value.
    public var element: Element? {
        if case .Next(let value) = self {
            return value
        }
        return nil
    }

    /// - returns: If `Error` event, returns error.
    public var error: ErrorType? {
        if case .Error(let error) = self {
            return error
        }
        return nil
    }
}
