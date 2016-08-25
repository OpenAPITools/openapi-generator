//
//  Errors.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/28/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

let RxErrorDomain       = "RxErrorDomain"
let RxCompositeFailures = "RxCompositeFailures"

/**
Generic Rx error codes.
*/
public enum RxError
    : ErrorType
    , CustomDebugStringConvertible {
    /**
    Unknown error occured.
    */
    case Unknown
    /**
    Performing an action on disposed object.
    */
    case Disposed(object: AnyObject)
    /**
    Aritmetic overflow error.
    */
    case Overflow
    /**
    Argument out of range error.
    */
    case ArgumentOutOfRange
    /**
    Sequence doesn't contain any elements.
    */
    case NoElements
    /**
    Sequence contains more than one element.
    */
    case MoreThanOneElement
    /**
     Timeout error.
     */
    case Timeout
}

public extension RxError {
    /**
     A textual representation of `self`, suitable for debugging.
    */
    public var debugDescription: String {
        switch self {
        case .Unknown:
            return "Unknown error occured."
        case .Disposed(let object):
            return "Object `\(object)` was already disposed."
        case .Overflow:
            return "Arithmetic overflow occured."
        case .ArgumentOutOfRange:
            return "Argument out of range."
        case .NoElements:
            return "Sequence doesn't contain any elements."
        case .MoreThanOneElement:
            return "Sequence contains more than one element."
        case .Timeout:
            return "Sequence timeout."
        }
    }
}