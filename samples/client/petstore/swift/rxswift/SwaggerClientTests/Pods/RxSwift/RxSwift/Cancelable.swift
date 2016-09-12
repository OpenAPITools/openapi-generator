//
//  Cancelable.swift
//  Rx
//
//  Created by Krunoslav Zaher on 3/12/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents disposable resource with state tracking.
*/
public protocol Cancelable : Disposable {
    /**
    - returns: Was resource disposed.
    */
    var disposed: Bool { get }
}