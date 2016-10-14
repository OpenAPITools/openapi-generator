//
//  NopDisposable.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/15/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Represents a disposable that does nothing on disposal.

Nop = No Operation
*/
public struct NopDisposable : Disposable {
 
    /**
    Singleton instance of `NopDisposable`.
    */
    @available(*, deprecated, renamed: "Disposables.create()")
    public static let instance: Disposable = NopDisposable()
    
    init() {
        
    }
    
    /**
    Does nothing.
    */
    public func dispose() {
    }
}
