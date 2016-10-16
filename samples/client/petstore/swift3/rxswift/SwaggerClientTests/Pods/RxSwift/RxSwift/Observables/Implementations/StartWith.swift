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

    override func run<O : ObserverType>(_ observer: O) -> Disposable where O.E == Element {
        for e in elements {
            observer.on(.next(e))
        }

        return source.subscribe(observer)
    }
}
