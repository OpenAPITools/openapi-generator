//
//  InfiniteSequence.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/13/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Sequence that repeats `repeatedValue` infinite number of times.
*/
struct InfiniteSequence<E> : SequenceType {
    typealias Element = E
    typealias Generator = AnyGenerator<E>

    private let _repeatedValue: E

    init(repeatedValue: E) {
        _repeatedValue = repeatedValue
    }

    func generate() -> Generator {
        let repeatedValue = _repeatedValue
        return AnyGenerator {
            return repeatedValue
        }
    }
}