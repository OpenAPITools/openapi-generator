//
//  AnonymousInvocable.swift
//  Rx
//
//  Created by Krunoslav Zaher on 11/7/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

struct AnonymousInvocable : InvocableType {
    private let _action: () -> ()

    init(_ action: () -> ()) {
        _action = action
    }

    func invoke() {
        _action()
    }
}