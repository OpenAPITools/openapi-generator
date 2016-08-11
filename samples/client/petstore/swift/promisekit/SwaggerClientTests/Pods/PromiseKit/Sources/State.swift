import Dispatch
import Foundation  // NSLog

enum Seal<R> {
    case Pending(Handlers<R>)
    case Resolved(R)
}

enum Resolution<T> {
    case Fulfilled(T)
    case Rejected(ErrorType, ErrorConsumptionToken)
}

// would be a protocol, but you can't have typed variables of “generic”
// protocols in Swift 2. That is, I couldn’t do var state: State<R> when
// it was a protocol. There is no work around.
class State<R> {
    func get() -> R? { fatalError("Abstract Base Class") }
    func get(body: (Seal<R>) -> Void) { fatalError("Abstract Base Class") }
}

class UnsealedState<R>: State<R> {
    private let barrier = dispatch_queue_create("org.promisekit.barrier", DISPATCH_QUEUE_CONCURRENT)
    private var seal: Seal<R>

    /**
     Quick return, but will not provide the handlers array because
     it could be modified while you are using it by another thread.
     If you need the handlers, use the second `get` variant.
    */
    override func get() -> R? {
        var result: R?
        dispatch_sync(barrier) {
            if case .Resolved(let resolution) = self.seal {
                result = resolution
            }
        }
        return result
    }

    override func get(body: (Seal<R>) -> Void) {
        var sealed = false
        dispatch_sync(barrier) {
            switch self.seal {
            case .Resolved:
                sealed = true
            case .Pending:
                sealed = false
            }
        }
        if !sealed {
            dispatch_barrier_sync(barrier) {
                switch (self.seal) {
                case .Pending:
                    body(self.seal)
                case .Resolved:
                    sealed = true  // welcome to race conditions
                }
            }
        }
        if sealed {
            body(seal)
        }
    }

    required init(inout resolver: ((R) -> Void)!) {
        seal = .Pending(Handlers<R>())
        super.init()
        resolver = { resolution in
            var handlers: Handlers<R>?
            dispatch_barrier_sync(self.barrier) {
                if case .Pending(let hh) = self.seal {
                    self.seal = .Resolved(resolution)
                    handlers = hh
                }
            }
            if let handlers = handlers {
                for handler in handlers {
                    handler(resolution)
                }
            }
        }
    }

    deinit {
        if case .Pending = seal {
            NSLog("PromiseKit: Pending Promise deallocated! This is usually a bug")
        }
    }
}

class SealedState<R>: State<R> {
    private let resolution: R
    
    init(resolution: R) {
        self.resolution = resolution
    }
    
    override func get() -> R? {
        return resolution
    }

    override func get(body: (Seal<R>) -> Void) {
        body(.Resolved(resolution))
    }
}


class Handlers<R>: SequenceType {
    var bodies: [(R)->Void] = []

    func append(body: (R)->Void) {
        bodies.append(body)
    }

    func generate() -> IndexingGenerator<[(R)->Void]> {
        return bodies.generate()
    }

    var count: Int {
        return bodies.count
    }
}


extension Resolution: CustomStringConvertible {
    var description: String {
        switch self {
        case .Fulfilled(let value):
            return "Fulfilled with value: \(value)"
        case .Rejected(let error):
            return "Rejected with error: \(error)"
        }
    }
}

extension UnsealedState: CustomStringConvertible {
    var description: String {
        var rv: String!
        get { seal in
            switch seal {
            case .Pending(let handlers):
                rv = "Pending with \(handlers.count) handlers"
            case .Resolved(let resolution):
                rv = "\(resolution)"
            }
        }
        return "UnsealedState: \(rv)"
    }
}

extension SealedState: CustomStringConvertible {
    var description: String {
        return "SealedState: \(resolution)"
    }
}
