//
//  Empty.swift
//  Rx
//
//  Created by Krunoslav Zaher on 8/30/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class Empty<Element> : Producer<Element> {
    override func subscribe<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        observer.on(.Completed)
        return NopDisposable.instance
    }
}