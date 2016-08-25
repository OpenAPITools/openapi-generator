//
//  StartWith.swift
//  RxCocoa
//
//  Created by Krunoslav Zaher on 4/6/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class StartWith<Element>: Producer<Element> {
    let elements: [Element]
    let source: Observable<Element>

    init(source: Observable<Element>, elements: [Element]) {
        self.source = source
        self.elements = elements
        super.init()
    }

    override func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        for e in elements {
            observer.on(.Next(e))
        }

        return source.subscribe(observer)
    }
}
