//
//  Producer.swift
//  Rx
//
//  Created by Krunoslav Zaher on 2/20/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

class Producer<Element> : Observable<Element> {
    override init() {
        super.init()
    }

    override func subscribe<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        if !CurrentThreadScheduler.isScheduleRequired {
            return run(observer)
        }
        else {
            return CurrentThreadScheduler.instance.schedule(()) { _ in
                return self.run(observer)
            }
        }
    }

    func run<O : ObserverType where O.E == Element>(observer: O) -> Disposable {
        abstractMethod()
    }
}