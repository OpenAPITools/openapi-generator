//
//  StableCompositeDisposable.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 6/12/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

public final class StableCompositeDisposable {
    @available(*, deprecated, renamed: "Disposables.create")
    public static func create(_ disposable1: Disposable, _ disposable2: Disposable) -> Disposable {
        return Disposables.create(disposable1, disposable2)
    }
}
