//
//  Debug.swift
//  RxSwift
//
//  Created by Krunoslav Zaher on 5/2/15.
//  Copyright © 2015 Krunoslav Zaher. All rights reserved.
//

import Foundation

let dateFormat = "yyyy-MM-dd HH:mm:ss.SSS"

func logEvent(identifier: String, dateFormat: NSDateFormatter, content: String) {
    print("\(dateFormat.stringFromDate(NSDate())): \(identifier) -> \(content)")
}

class Debug_<O: ObserverType> : Sink<O>, ObserverType {
    typealias Element = O.E
    typealias Parent = Debug<Element>

    private let _parent: Parent
    private let _timestampFormatter = NSDateFormatter()

    init(parent: Parent, observer: O) {
        _parent = parent
        _timestampFormatter.dateFormat = dateFormat

        logEvent(_parent._identifier, dateFormat: _timestampFormatter, content: "subscribed")

        super.init(observer: observer)
    }

    func on(event: Event<Element>) {
        let maxEventTextLength = 40
        let eventText = "\(event)"
        let eventNormalized = eventText.characters.count > maxEventTextLength
            ? String(eventText.characters.prefix(maxEventTextLength / 2)) + "..." + String(eventText.characters.suffix(maxEventTextLength / 2))
            : eventText

        logEvent(_parent._identifier, dateFormat: _timestampFormatter, content: "Event \(eventNormalized)")
        forwardOn(event)
    }

    override func dispose() {
        logEvent(_parent._identifier, dateFormat: _timestampFormatter, content: "disposed")
        super.dispose()
    }
}

class Debug<Element> : Producer<Element> {
    private let _identifier: String

    private let _source: Observable<Element>

    init(source: Observable<Element>, identifier: String?, file: String, line: UInt, function: String) {
        if let identifier = identifier {
            _identifier = identifier
        }
        else {
            let trimmedFile: String
            if let lastIndex = file.lastIndexOf("/") {
                trimmedFile = file[lastIndex.successor() ..< file.endIndex]
            }
            else {
                trimmedFile = file
            }
            _identifier = "\(trimmedFile):\(line) (\(function))"
        }
        _source = source
    }

    override func run<O: ObserverType where O.E == Element>(observer: O) -> Disposable {
        let sink = Debug_(parent: self, observer: observer)
        sink.disposable = _source.subscribe(sink)
        return sink
    }
}