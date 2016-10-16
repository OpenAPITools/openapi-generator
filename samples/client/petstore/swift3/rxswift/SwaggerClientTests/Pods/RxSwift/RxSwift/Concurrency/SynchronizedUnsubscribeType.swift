//
//  SynchronizedUnsubscribeType.swift
//  Rx
//
//  Created by Krunoslav Zaher on 10/25/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

protocol SynchronizedUnsubscribeType : class {
    associatedtype DisposeKey

    func synchronizedUnsubscribe(_ disposeKey: DisposeKey)
}
