//
//  StableCompositeDisposable.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/12/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

public final class StableCompositeDisposable {
    public static func create(disposable1: Disposable, _ disposable2: Disposable) -> Disposable {
        return BinaryDisposable(disposable1, disposable2)
    }
}