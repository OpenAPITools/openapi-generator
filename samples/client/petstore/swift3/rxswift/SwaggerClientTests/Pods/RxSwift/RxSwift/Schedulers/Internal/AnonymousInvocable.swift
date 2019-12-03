//
//  AnonymousInvocable.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 11/7/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

struct AnonymousInvocable: InvocableType {
    private let _action: () -> Void

    init(_ action: @escaping () -> Void) {
        _action = action
    }

    func invoke() {
        _action()
    }
}
