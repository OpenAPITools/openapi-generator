import Dispatch
import Foundation.NSError

/**
 A *promise* represents the future value of a task.

 To obtain the value of a promise we call `then`.

 Promises are chainable: `then` returns a promise, you can call `then` on
 that promise, which returns a promise, you can call `then` on that
 promise, et cetera.

 Promises start in a pending state and *resolve* with a value to become
 *fulfilled* or with an `ErrorType` to become rejected.

 - SeeAlso: [PromiseKit `then` Guide](http://promisekit.org/then/)
 - SeeAlso: [PromiseKit Chaining Guide](http://promisekit.org/chaining/)
*/
public class Promise<T> {
    let state: State<Resolution<T>>

    /**
     Create a new pending promise.

     Use this method when wrapping asynchronous systems that do *not* use
     promises so that they can be involved in promise chains.

     Don’t use this method if you already have promises! Instead, just return
     your promise!

     The closure you pass is executed immediately on the calling thread.

         func fetchKitten() -> Promise<UIImage> {
             return Promise { fulfill, reject in
                 KittenFetcher.fetchWithCompletionBlock({ img, err in
                     if err == nil {
                         if img.size.width > 0 {
                             fulfill(img)
                         } else {
                             reject(Error.ImageTooSmall)
                         }
                     } else {
                         reject(err)
                     }
                 })
             }
         }

     - Parameter resolvers: The provided closure is called immediately.
     Inside, execute your asynchronous system, calling fulfill if it suceeds
     and reject for any errors.

     - Returns: return A new promise.

     - Note: If you are wrapping a delegate-based system, we recommend
     to use instead: Promise.pendingPromise()

     - SeeAlso: http://promisekit.org/sealing-your-own-promises/
     - SeeAlso: http://promisekit.org/wrapping-delegation/
     - SeeAlso: init(resolver:)
    */
    public init(@noescape resolvers: (fulfill: (T) -> Void, reject: (ErrorType) -> Void) throws -> Void) {
        var resolve: ((Resolution<T>) -> Void)!
        state = UnsealedState(resolver: &resolve)
        do {
            try resolvers(fulfill: { resolve(.Fulfilled($0)) }, reject: { error in
                if self.pending {
                    resolve(.Rejected(error, ErrorConsumptionToken(error)))
                } else {
                    NSLog("PromiseKit: Warning: reject called on already rejected Promise: %@", "\(error)")
                }
            })
        } catch {
            resolve(.Rejected(error, ErrorConsumptionToken(error)))
        }
    }

    /**
     Create a new pending promise.

     This initializer is convenient when wrapping asynchronous systems that
     use common patterns. For example:

         func fetchKitten() -> Promise<UIImage> {
             return Promise { resolve in
                 KittenFetcher.fetchWithCompletionBlock(resolve)
             }
         }

     - SeeAlso: init(resolvers:)
    */
    public convenience init(@noescape resolver: ((T?, NSError?) -> Void) throws -> Void) {
        self.init(sealant: { resolve in
            try resolver { obj, err in
                if let obj = obj {
                    resolve(.Fulfilled(obj))
                } else if let err = err {
                    resolve(.Rejected(err, ErrorConsumptionToken(err as ErrorType)))
                } else {
                    resolve(.Rejected(Error.DoubleOhSux0r, ErrorConsumptionToken(Error.DoubleOhSux0r)))
                }
            }
        })
    }

    /**
     Create a new pending promise.

     This initializer is convenient when wrapping asynchronous systems that
     use common patterns. For example:

         func fetchKitten() -> Promise<UIImage> {
             return Promise { resolve in
                 KittenFetcher.fetchWithCompletionBlock(resolve)
             }
         }

     - SeeAlso: init(resolvers:)
    */
    public convenience init(@noescape resolver: ((T, NSError?) -> Void) throws -> Void) {
        self.init(sealant: { resolve in
            try resolver { obj, err in
                if let err = err {
                    resolve(.Rejected(err, ErrorConsumptionToken(err)))
                } else {
                    resolve(.Fulfilled(obj))
                }
            }
        })
    }

    /**
     Create a new fulfilled promise.
    */
    public init(_ value: T) {
        state = SealedState(resolution: .Fulfilled(value))
    }

    @available(*, unavailable, message="T cannot conform to ErrorType")
    public init<T: ErrorType>(_ value: T) { abort() }

    /**
     Create a new rejected promise.
    */
    public init(error: ErrorType) {
        /**
          Implementation note, the error label is necessary to prevent:

             let p = Promise(ErrorType())

          Resulting in Promise<ErrorType>. The above @available annotation
          does not help for some reason. A work-around is:

             let p: Promise<Void> = Promise(ErrorType())
        
          But I can’t expect users to do this.
        */
        state = SealedState(resolution: .Rejected(error, ErrorConsumptionToken(error)))
    }

    /**
     Careful with this, it is imperative that sealant can only be called once
     or you will end up with spurious unhandled-errors due to possible double
     rejections and thus immediately deallocated ErrorConsumptionTokens.
    */
    init(@noescape sealant: ((Resolution<T>) -> Void) throws -> Void) {
        var resolve: ((Resolution<T>) -> Void)!
        state = UnsealedState(resolver: &resolve)
        do {
            try sealant(resolve)
        } catch {
            resolve(.Rejected(error, ErrorConsumptionToken(error)))
        }
    }

    /**
     Making promises that wrap asynchronous delegation systems or other larger asynchronous systems without a simple completion handler is easier with pendingPromise.

         class Foo: BarDelegate {
             let (promise, fulfill, reject) = Promise<Int>.pendingPromise()
    
             func barDidFinishWithResult(result: Int) {
                 fulfill(result)
             }
    
             func barDidError(error: NSError) {
                 reject(error)
             }
         }

     - Returns: A tuple consisting of: 
       1) A promise
       2) A function that fulfills that promise
       3) A function that rejects that promise
    */
    public class func pendingPromise() -> (promise: Promise, fulfill: (T) -> Void, reject: (ErrorType) -> Void) {
        var fulfill: ((T) -> Void)!
        var reject: ((ErrorType) -> Void)!
        let promise = Promise { fulfill = $0; reject = $1 }
        return (promise, fulfill, reject)
    }

    func pipe(body: (Resolution<T>) -> Void) {
        state.get { seal in
            switch seal {
            case .Pending(let handlers):
                handlers.append(body)
            case .Resolved(let resolution):
                body(resolution)
            }
        }
    }

    private convenience init<U>(when: Promise<U>, body: (Resolution<U>, (Resolution<T>) -> Void) -> Void) {
        self.init { resolve in
            when.pipe { resolution in
                body(resolution, resolve)
            }
        }
    }

    /**
     The provided closure is executed when this Promise is resolved.

     - Parameter on: The queue on which body should be executed.
     - Parameter body: The closure that is executed when this Promise is fulfilled.
     - Returns: A new promise that is resolved with the value returned from the provided closure. For example:

           NSURLConnection.GET(url).then { (data: NSData) -> Int in
               //…
               return data.length
           }.then { length in
               //…
           }

     - SeeAlso: `thenInBackground`
    */
    public func then<U>(on q: dispatch_queue_t = dispatch_get_main_queue(), _ body: (T) throws -> U) -> Promise<U> {
        return Promise<U>(when: self) { resolution, resolve in
            switch resolution {
            case .Rejected(let error):
                resolve(.Rejected(error))
            case .Fulfilled(let value):
                contain_zalgo(q, rejecter: resolve) {
                    resolve(.Fulfilled(try body(value)))
                }
            }
        }
    }

    /**
     The provided closure is executed when this Promise is resolved.

     - Parameter on: The queue on which body should be executed.
     - Parameter body: The closure that is executed when this Promise is fulfilled.
     - Returns: A new promise that is resolved when the Promise returned from the provided closure resolves. For example:

           NSURLSession.GET(url1).then { (data: NSData) -> Promise<NSData> in
               //…
               return NSURLSession.GET(url2)
           }.then { data in
               //…
           }

     - SeeAlso: `thenInBackground`
    */
    public func then<U>(on q: dispatch_queue_t = dispatch_get_main_queue(), _ body: (T) throws -> Promise<U>) -> Promise<U> {
        return Promise<U>(when: self) { resolution, resolve in
            switch resolution {
            case .Rejected(let error):
                resolve(.Rejected(error))
            case .Fulfilled(let value):
                contain_zalgo(q, rejecter: resolve) {
                    let promise = try body(value)
                    guard promise !== self else { throw Error.ReturnedSelf }
                    promise.pipe(resolve)
                }
            }
        }
    }

    @available(*, unavailable)
    public func then<U>(on: dispatch_queue_t = dispatch_get_main_queue(), _ body: (T) throws -> Promise<U>?) -> Promise<U> { abort() }

    /**
     The provided closure is executed when this Promise is resolved.

     - Parameter on: The queue on which body should be executed.
     - Parameter body: The closure that is executed when this Promise is fulfilled.
     - Returns: A new promise that is resolved when the AnyPromise returned from the provided closure resolves. For example:

           NSURLSession.GET(url).then { (data: NSData) -> AnyPromise in
               //…
               return SCNetworkReachability()
           }.then { _ in
               //…
           }

     - SeeAlso: `thenInBackground`
    */
    public func then(on q: dispatch_queue_t = dispatch_get_main_queue(), body: (T) throws -> AnyPromise) -> Promise<AnyObject?> {
        return Promise<AnyObject?>(when: self) { resolution, resolve in
            switch resolution {
            case .Rejected(let error):
                resolve(.Rejected(error))
            case .Fulfilled(let value):
                contain_zalgo(q, rejecter: resolve) {
                    try body(value).pipe(resolve)
                }
            }
        }
    }

    @available(*, unavailable)
    public func then(on: dispatch_queue_t = dispatch_get_main_queue(), body: (T) throws -> AnyPromise?) -> Promise<AnyObject?> { abort() }

    /**
     The provided closure is executed on the default background queue when this Promise is fulfilled.

     This method is provided as a convenience for `then`.

     - SeeAlso: `then`
    */
    public func thenInBackground<U>(body: (T) throws -> U) -> Promise<U> {
        return then(on: dispatch_get_global_queue(0, 0), body)
    }

    /**
     The provided closure is executed on the default background queue when this Promise is fulfilled.

     This method is provided as a convenience for `then`.

     - SeeAlso: `then`
    */
    public func thenInBackground<U>(body: (T) throws -> Promise<U>) -> Promise<U> {
        return then(on: dispatch_get_global_queue(0, 0), body)
    }

    @available(*, unavailable)
    public func thenInBackground<U>(body: (T) throws -> Promise<U>?) -> Promise<U> { abort() }

    /**
     The provided closure is executed when this promise is rejected.

     Rejecting a promise cascades: rejecting all subsequent promises (unless
     recover is invoked) thus you will typically place your catch at the end
     of a chain. Often utility promises will not have a catch, instead
     delegating the error handling to the caller.

     The provided closure always runs on the main queue.

     - Parameter policy: The default policy does not execute your handler for cancellation errors. See registerCancellationError for more documentation.
     - Parameter body: The handler to execute if this promise is rejected.
     - SeeAlso: `registerCancellationError`
    */
    public func error(policy policy: ErrorPolicy = .AllErrorsExceptCancellation, _ body: (ErrorType) -> Void) {
        
        func consume(error: ErrorType, _ token: ErrorConsumptionToken) {
            token.consumed = true
            body(error)
        }

        pipe { resolution in
            switch (resolution, policy) {
            case (let .Rejected(error, token), .AllErrorsExceptCancellation):
                dispatch_async(dispatch_get_main_queue()) {
                    guard let cancellableError = error as? CancellableErrorType where cancellableError.cancelled else {
                        consume(error, token)
                        return
                    }
                }
            case (let .Rejected(error, token), _):
                dispatch_async(dispatch_get_main_queue()) {
                    consume(error, token)
                }
            case (.Fulfilled, _):
                break
            }
        }
    }

    /**
     The provided closure is executed when this promise is rejected giving you
     an opportunity to recover from the error and continue the promise chain.
    */
    public func recover(on q: dispatch_queue_t = dispatch_get_main_queue(), _ body: (ErrorType) throws -> Promise) -> Promise {
        return Promise(when: self) { resolution, resolve in
            switch resolution {
            case .Rejected(let error, let token):
                contain_zalgo(q, rejecter: resolve) {
                    token.consumed = true
                    let promise = try body(error)
                    guard promise !== self else { throw Error.ReturnedSelf }
                    promise.pipe(resolve)
                }
            case .Fulfilled:
                resolve(resolution)
            }
        }
    }

    @available(*, unavailable)
    public func recover(on: dispatch_queue_t = dispatch_get_main_queue(), _ body: (ErrorType) throws -> Promise?) -> Promise { abort() }

    public func recover(on q: dispatch_queue_t = dispatch_get_main_queue(), _ body: (ErrorType) throws -> T) -> Promise {
        return Promise(when: self) { resolution, resolve in
            switch resolution {
            case .Rejected(let error, let token):
                contain_zalgo(q, rejecter: resolve) {
                    token.consumed = true
                    resolve(.Fulfilled(try body(error)))
                }
            case .Fulfilled:
                resolve(resolution)
            }
        }
    }

    /**
     The provided closure is executed when this Promise is resolved.

         UIApplication.sharedApplication().networkActivityIndicatorVisible = true
         somePromise().then {
             //…
         }.always {
             UIApplication.sharedApplication().networkActivityIndicatorVisible = false
         }

     - Parameter on: The queue on which body should be executed.
     - Parameter body: The closure that is executed when this Promise is resolved.
    */
    public func always(on q: dispatch_queue_t = dispatch_get_main_queue(), _ body: () -> Void) -> Promise {
        return Promise(when: self) { resolution, resolve in
            contain_zalgo(q) {
                body()
                resolve(resolution)
            }
        }
    }

    @available(*, unavailable, renamed="ensure")
    public func finally(on: dispatch_queue_t = dispatch_get_main_queue(), body: () -> Void) -> Promise { abort() }

    @available(*, unavailable, renamed="report")
    public func catch_(policy policy: ErrorPolicy = .AllErrorsExceptCancellation, body: () -> Void) -> Promise { abort() }

    @available(*, unavailable, renamed="pendingPromise")
    public class func defer_() -> (promise: Promise, fulfill: (T) -> Void, reject: (ErrorType) -> Void) { abort() }

    @available(*, deprecated, renamed="error")
    public func report(policy policy: ErrorPolicy = .AllErrorsExceptCancellation, _ body: (ErrorType) -> Void) { error(policy: policy, body) }

    @available(*, deprecated, renamed="always")
    public func ensure(on q: dispatch_queue_t = dispatch_get_main_queue(), _ body: () -> Void) -> Promise { return always(on: q, body) }
}


/**
 Zalgo is dangerous.

 Pass as the `on` parameter for a `then`. Causes the handler to be executed
 as soon as it is resolved. That means it will be executed on the queue it
 is resolved. This means you cannot predict the queue.

 In the case that the promise is already resolved the handler will be
 executed immediately.

 zalgo is provided for libraries providing promises that have good tests
 that prove unleashing zalgo is safe. You can also use it in your
 application code in situations where performance is critical, but be
 careful: read the essay at the provided link to understand the risks.

 - SeeAlso: http://blog.izs.me/post/59142742143/designing-apis-for-asynchrony
*/
public let zalgo: dispatch_queue_t = dispatch_queue_create("Zalgo", nil)

/**
 Waldo is dangerous.

 Waldo is zalgo, unless the current queue is the main thread, in which case
 we dispatch to the default background queue.

 If your block is likely to take more than a few milliseconds to execute,
 then you should use waldo: 60fps means the main thread cannot hang longer
 than 17 milliseconds. Don’t contribute to UI lag.

 Conversely if your then block is trivial, use zalgo: GCD is not free and
 for whatever reason you may already be on the main thread so just do what
 you are doing quickly and pass on execution.

 It is considered good practice for asynchronous APIs to complete onto the
 main thread. Apple do not always honor this, nor do other developers.
 However, they *should*. In that respect waldo is a good choice if your
 then is going to take a while and doesn’t interact with the UI.

 Please note (again) that generally you should not use zalgo or waldo. The
 performance gains are neglible and we provide these functions only out of
 a misguided sense that library code should be as optimized as possible.
 If you use zalgo or waldo without tests proving their correctness you may
 unwillingly introduce horrendous, near-impossible-to-trace bugs.

 - SeeAlso: zalgo
*/
public let waldo: dispatch_queue_t = dispatch_queue_create("Waldo", nil)

func contain_zalgo(q: dispatch_queue_t, block: () -> Void) {
    if q === zalgo {
        block()
    } else if q === waldo {
        if NSThread.isMainThread() {
            dispatch_async(dispatch_get_global_queue(0, 0), block)
        } else {
            block()
        }
    } else {
        dispatch_async(q, block)
    }
}

func contain_zalgo<T>(q: dispatch_queue_t, rejecter resolve: (Resolution<T>) -> Void, block: () throws -> Void) {
    contain_zalgo(q) {
        do {
            try block()
        } catch {
            resolve(.Rejected(error, ErrorConsumptionToken(error)))
        }
    }
}


extension Promise {
    /**
     Void promises are less prone to generics-of-doom scenarios.
     - SeeAlso: when.swift contains enlightening examples of using `Promise<Void>` to simplify your code.
    */
    public func asVoid() -> Promise<Void> {
        return then(on: zalgo) { _ in return }
    }
}


extension Promise: CustomStringConvertible {
    public var description: String {
        return "Promise: \(state)"
    }
}

/**
 `firstly` can make chains more readable.

 Compare:

     NSURLConnection.GET(url1).then {
         NSURLConnection.GET(url2)
     }.then {
         NSURLConnection.GET(url3)
     }

 With:

     firstly {
         NSURLConnection.GET(url1)
     }.then {
         NSURLConnection.GET(url2)
     }.then {
         NSURLConnection.GET(url3)
     }
*/
public func firstly<T>(@noescape promise: () throws -> Promise<T>) -> Promise<T> {
    do {
        return try promise()
    } catch {
        return Promise(error: error)
    }
}

/**
 `firstly` can make chains more readable.

 Compare:

     SCNetworkReachability().then {
         NSURLSession.GET(url2)
     }.then {
         NSURLSession.GET(url3)
     }

 With:

     firstly {
         SCNetworkReachability()
     }.then {
         NSURLSession.GET(url2)
     }.then {
         NSURLSession.GET(url3)
     }
*/
public func firstly(@noescape promise: () throws -> AnyPromise) -> Promise<AnyObject?> {
    return Promise { resolve in
        try promise().pipe(resolve)
    }
}

@available(*, unavailable, message="Instead, throw")
public func firstly<T: ErrorType>(@noescape promise: () throws -> Promise<T>) -> Promise<T> {
    fatalError("Unavailable function")
}


public enum ErrorPolicy {
    case AllErrors
    case AllErrorsExceptCancellation
}


extension AnyPromise {
    private func pipe(resolve: (Resolution<AnyObject?>) -> Void) -> Void {
        pipe { (obj: AnyObject?) in
            if let error = obj as? NSError {
                resolve(.Rejected(error, ErrorConsumptionToken(error)))
            } else {
                // possibly the value of this promise is a PMKManifold, if so
                // calling the objc `value` method will return the first item.
                resolve(.Fulfilled(self.valueForKey("value")))
            }
        }
    }
}


extension Promise {
    @available(*, unavailable, message="T cannot conform to ErrorType")
    public convenience init<T: ErrorType>(@noescape resolvers: (fulfill: (T) -> Void, reject: (ErrorType) -> Void) throws -> Void) { abort() }

    @available(*, unavailable, message="T cannot conform to ErrorType")
    public convenience init<T: ErrorType>(@noescape resolver: ((T?, NSError?) -> Void) throws -> Void) { abort() }

    @available(*, unavailable, message="T cannot conform to ErrorType")
    public convenience init<T: ErrorType>(@noescape resolver: ((T, NSError?) -> Void) throws -> Void) { abort() }

    @available(*, unavailable, message="T cannot conform to ErrorType")
    public class func pendingPromise<T: ErrorType>() -> (promise: Promise, fulfill: (T) -> Void, reject: (ErrorType) -> Void) { abort() }

    @available (*, unavailable, message="U cannot conform to ErrorType")
    public func then<U: ErrorType>(on: dispatch_queue_t = dispatch_get_main_queue(), _ body: (T) throws -> U) -> Promise<U> { abort() }

    @available (*, unavailable, message="U cannot conform to ErrorType")
    public func then<U: ErrorType>(on: dispatch_queue_t = dispatch_get_main_queue(), _ body: (T) throws -> Promise<U>) -> Promise<U> { abort() }

    @available(*, unavailable, message="U cannot conform to ErrorType")
    public func thenInBackground<U: ErrorType>(body: (T) throws -> U) -> Promise<U> { abort() }

    @available(*, unavailable, message="U cannot conform to ErrorType")
    public func thenInBackground<U: ErrorType>(body: (T) throws -> Promise<U>) -> Promise<U> { abort() }
}
