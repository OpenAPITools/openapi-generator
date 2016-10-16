//
//  DisposeBase.swift
//  Rx
//
//  Created by Krunoslav Zaher on 4/4/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Base class for all disposables.
*/
public class DisposeBase {
    init() {
#if TRACE_RESOURCES
    let _ = AtomicIncrement(&resourceCount)
#endif
    }
    
    deinit {
#if TRACE_RESOURCES
    let _ = AtomicDecrement(&resourceCount)
#endif
    }
}
