//
//  DispatchQueueSchedulerQOS.swift
//  RxSwift
//
//  Created by John C. "Hsoi" Daub on 12/30/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

/**
Identifies one of the global concurrent dispatch queues with specified quality of service class.
*/
public enum DispatchQueueSchedulerQOS {
    
    /**
    Identifies global dispatch queue with `QOS_CLASS_USER_INTERACTIVE`
    */
    case userInteractive
    
    /**
    Identifies global dispatch queue with `QOS_CLASS_USER_INITIATED`
    */
    case userInitiated
    
    /**
    Identifies global dispatch queue with `QOS_CLASS_DEFAULT`
    */
    case `default`

    /**
     Identifies global dispatch queue with `QOS_CLASS_UTILITY`
     */
    case utility
    
    /**
     Identifies global dispatch queue with `QOS_CLASS_BACKGROUND`
     */
    case background
}


@available(iOS 8, OSX 10.10, *)
extension DispatchQueueSchedulerQOS {
    var qos: DispatchQoS {
        switch self {
        case .userInteractive: return .userInteractive
        case .userInitiated:   return .userInitiated
        case .default:         return .default
        case .utility:         return .utility
        case .background:      return .background
        }
    }
}
