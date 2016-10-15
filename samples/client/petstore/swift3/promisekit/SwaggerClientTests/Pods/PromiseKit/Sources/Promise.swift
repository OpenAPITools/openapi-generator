import class Dispatch.DispatchQueue
import class Foundation.NSError
import func Foundation.NSLog


/**
 A *promise* represents the future value of a (usually) asynchronous task.

 To obtain the value of a promise we call `then`.

 Promises are chainable: `then` returns a promise, you can call `then` on
 that promise, which returns a promise, you can call `then` on that
 promise, et cetera.

 Promises start in a pending state and *resolve* with a value to become
 *fulfilled* or an `Error` to become rejected.

 - SeeAlso: [PromiseKit `then` Guide](http://promisekit.org/docs/)
 */
open class Promise<T> {
    let state: State<T>

    /**
     Create a new, pending promise.

         func fetchAvatar(user: String) -> Promise<UIImage> {
             return Promise { fulfill, reject in
                 MyWebHelper.GET("\(user)/avatar") { data, err in
                     guard let data = data else { return reject(err) }
                     guard let img = UIImage(data: data) else { return reject(MyError.InvalidImage) }
                     guard let img.size.width > 0 else { return reject(MyError.ImageTooSmall) }
                     fulfill(img)
                 }
             }
         }

     - Parameter resolvers: The provided closure is called immediately on the active thread; commence your asynchronous task, calling either fulfill or reject when it completes.
      - Parameter fulfill: Fulfills this promise with the provided value.
      - Parameter reject: Rejects this promise with the provided error.

     - Returns: A new promise.

     - Note: If you are wrapping a delegate-based system, we recommend
     to use instead: `Promise.pending()`

     - SeeAlso: http://promisekit.org/docs/sealing-promises/
     - SeeAlso: http://promisekit.org/docs/cookbook/wrapping-delegation/
     - SeeAlso: pending()
     */
    required public init(resolvers: (_ fulfill: @escaping (T) -> Void, _ reject: @escaping (Error) -> Void) throws -> Void) {
        var resolve: ((Resolution<T>) -> Void)!
        do {
            state = UnsealedState(resolver: &resolve)
            try resolvers({ resolve(.fulfilled($0)) }, { error in
                #if !PMKDisableWarnings
                    if self.isPending {
                        resolve(Resolution(error))
                    } else {
                        NSLog("PromiseKit: warning: reject called on already rejected Promise: \(error)")
                    }
                #else
                    resolve(Resolution(error))
                #endif
            })
        } catch {
            resolve(Resolution(error))
        }
    }

    /**
     Create an already fulfilled promise.
     */
    required public init(value: T) {
        state = SealedState(resolution: .fulfilled(value))
    }

    /**
     Create an already rejected promise.
     */
    required public init(error: Error) {
        state = SealedState(resolution: Resolution(error))
    }

    /**
     Careful with this, it is imperative that sealant can only be called once
     or you will end up with spurious unhandled-errors due to possible double
     rejections and thus immediately deallocated ErrorConsumptionTokens.
     */
    init(sealant: (@escaping (Resolution<T>) -> Void) -> Void) {
        var resolve: ((Resolution<T>) -> Void)!
        state = UnsealedState(resolver: &resolve)
        sealant(resolve)
    }

    /**
     A `typealias` for the return values of `pending()`. Simplifies declaration of properties that reference the values' containing tuple when this is necessary. For example, when working with multiple `pendingPromise(value: ())`s within the same scope, or when the promise initialization must occur outside of the caller's initialization.

         class Foo: BarDelegate {
            var task: Promise<Int>.PendingTuple?
         }

     - SeeAlso: pending()
     */
    public typealias PendingTuple = (promise: Promise, fulfill: (T) -> Void, reject: (Error) -> Void)

    /**
     Making promises that wrap asynchronous delegation systems or other larger asynchronous systems without a simple completion handler is easier with pending.

         class Foo: BarDelegate {
             let (promise, fulfill, reject) = Promise<Int>.pending()
    
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
    public final class func pending() -> PendingTuple {
        var fulfill: ((T) -> Void)!
        var reject: ((Error) -> Void)!
        let promise = self.init { fulfill = $0; reject = $1 }
        return (promise, fulfill, reject)
    }

    /**
     The provided closure is executed when this promise is resolved.

     - Parameter on: The queue to which the provided closure dispatches.
     - Parameter body: The closure that is executed when this Promise is fulfilled.
     - Returns: A new promise that is resolved with the value returned from the provided closure. For example:

           NSURLSession.GET(url).then { data -> Int in
               //…
               return data.length
           }.then { length in
               //…
           }
     */
    public func then<U>(on q: DispatchQueue = .default, execute body: @escaping (T) throws -> U) -> Promise<U> {
        return Promise<U> { resolve in
            state.then(on: q, else: resolve) { value in
                resolve(.fulfilled(try body(value)))
            }
        }
    }

    /**
     The provided closure executes when this promise resolves.
     
     This variant of `then` allows chaining promises, the promise returned by the provided closure is resolved before the promise returned by this closure resolves.

     - Parameter on: The queue to which the provided closure dispatches.
     - Parameter execute: The closure that executes when this promise fulfills.
     - Returns: A new promise that resolves when the promise returned from the provided closure resolves. For example:

           URLSession.GET(url1).then { data in
               return CLLocationManager.promise()
           }.then { location in
               //…
           }
     */
    public func then<U>(on q: DispatchQueue = .default, execute body: @escaping (T) throws -> Promise<U>) -> Promise<U> {
        var rv: Promise<U>!
        rv = Promise<U> { resolve in
            state.then(on: q, else: resolve) { value in
                let promise = try body(value)
                guard promise !== rv else { throw PMKError.returnedSelf }
                promise.state.pipe(resolve)
            }
        }
        return rv
    }

    /**
     The provided closure executes when this promise rejects.

     Rejecting a promise cascades: rejecting all subsequent promises (unless
     recover is invoked) thus you will typically place your catch at the end
     of a chain. Often utility promises will not have a catch, instead
     delegating the error handling to the caller.

     - Parameter on: The queue to which the provided closure dispatches.
     - Parameter policy: The default policy does not execute your handler for cancellation errors.
     - Parameter execute: The handler to execute if this promise is rejected.
     - Returns: `self`
     - SeeAlso: [Cancellation](http://promisekit.org/docs/)
     - Important: The promise that is returned is `self`. `catch` cannot affect the chain, in PromiseKit 3 no promise was returned to strongly imply this, however for PromiseKit 4 we started returning a promise so that you can `always` after a catch or return from a function that has an error handler.
     */
    @discardableResult
    public func `catch`(on q: DispatchQueue = .default, policy: CatchPolicy = .allErrorsExceptCancellation, execute body: @escaping (Error) -> Void) -> Promise {
        state.catch(on: q, policy: policy, else: { _ in }, execute: body)
        return self
    }

    /**
     The provided closure executes when this promise rejects.
     
     Unlike `catch`, `recover` continues the chain provided the closure does not throw. Use `recover` in circumstances where recovering the chain from certain errors is a possibility. For example:
     
         CLLocationManager.promise().recover { error in
             guard error == CLError.unknownLocation else { throw error }
             return CLLocation.Chicago
         }

     - Parameter on: The queue to which the provided closure dispatches.
     - Parameter policy: The default policy does not execute your handler for cancellation errors.
     - Parameter execute: The handler to execute if this promise is rejected.
     - SeeAlso: [Cancellation](http://promisekit.org/docs/)
     */
    public func recover(on q: DispatchQueue = .default, policy: CatchPolicy = .allErrorsExceptCancellation, execute body: @escaping (Error) throws -> Promise) -> Promise {
        var rv: Promise!
        rv = Promise { resolve in
            state.catch(on: q, policy: policy, else: resolve) { error in
                let promise = try body(error)
                guard promise !== rv else { throw PMKError.returnedSelf }
                promise.state.pipe(resolve)
            }
        }
        return rv
    }

    /**
     The provided closure executes when this promise rejects.

     Unlike `catch`, `recover` continues the chain provided the closure does not throw. Use `recover` in circumstances where recovering the chain from certain errors is a possibility. For example:

         CLLocationManager.promise().recover { error in
             guard error == CLError.unknownLocation else { throw error }
             return CLLocation.Chicago
         }

     - Parameter on: The queue to which the provided closure dispatches.
     - Parameter policy: The default policy does not execute your handler for cancellation errors.
     - Parameter execute: The handler to execute if this promise is rejected.
     - SeeAlso: [Cancellation](http://promisekit.org/docs/)
     */
    public func recover(on q: DispatchQueue = .default, policy: CatchPolicy = .allErrorsExceptCancellation, execute body: @escaping (Error) throws -> T) -> Promise {
        return Promise { resolve in
            state.catch(on: q, policy: policy, else: resolve) { error in
                resolve(.fulfilled(try body(error)))
            }
        }
    }

    /**
     The provided closure executes when this promise resolves.

         firstly {
             UIApplication.shared.networkActivityIndicatorVisible = true
         }.then {
             //…
         }.always {
             UIApplication.shared.networkActivityIndicatorVisible = false
         }.catch {
             //…
         }

     - Parameter on: The queue to which the provided closure dispatches.
     - Parameter execute: The closure that executes when this promise resolves.
     - Returns: A new promise, resolved with this promise’s resolution.
     */
    public func always(on q: DispatchQueue = .default, execute body: @escaping () -> Void) -> Promise {
        state.always(on: q) { resolution in
            body()
        }
        return self
    }

    /**
     `tap` allows you to “tap” into a promise chain and inspect its result.
     
     The function you provide cannot mutate the chain.
 
         NSURLSession.GET(/*…*/).tap { result in
             print(result)
         }

     - Parameter on: The queue to which the provided closure dispatches.
     - Parameter execute: The closure that executes when this promise resolves.
     - Returns: A new promise, resolved with this promise’s resolution.
     */
    @discardableResult
    public func tap(on q: DispatchQueue = .default, execute body: @escaping (Result<T>) -> Void) -> Promise {
        state.always(on: q) { resolution in
            body(Result(resolution))
        }
        return self
    }

    /**
     Void promises are less prone to generics-of-doom scenarios.
     - SeeAlso: when.swift contains enlightening examples of using `Promise<Void>` to simplify your code.
     */
    public func asVoid() -> Promise<Void> {
        return then(on: zalgo) { _ in return }
    }

//MARK: deprecations

    @available(*, unavailable, renamed: "always()")
    public func finally(on: DispatchQueue = DispatchQueue.main, execute body: () -> Void) -> Promise { fatalError() }

    @available(*, unavailable, renamed: "always()")
    public func ensure(on: DispatchQueue = DispatchQueue.main, execute body: () -> Void) -> Promise { fatalError() }

    @available(*, unavailable, renamed: "pending()")
    public class func `defer`() -> PendingTuple { fatalError() }

    @available(*, unavailable, renamed: "pending()")
    public class func `pendingPromise`() -> PendingTuple { fatalError() }

    @available(*, unavailable, message: "deprecated: use then(on: .global())")
    public func thenInBackground<U>(execute body: (T) throws -> U) -> Promise<U> { fatalError() }

    @available(*, unavailable, renamed: "catch")
    public func onError(policy: CatchPolicy = .allErrors, execute body: (Error) -> Void) { fatalError() }

    @available(*, unavailable, renamed: "catch")
    public func errorOnQueue(_ on: DispatchQueue, policy: CatchPolicy = .allErrors, execute body: (Error) -> Void) { fatalError() }

    @available(*, unavailable, renamed: "catch")
    public func error(policy: CatchPolicy, execute body: (Error) -> Void) { fatalError() }

    @available(*, unavailable, renamed: "catch")
    public func report(policy: CatchPolicy = .allErrors, execute body: (Error) -> Void) { fatalError() }

//MARK: disallow `Promise<Error>`

    @available(*, unavailable, message: "cannot instantiate Promise<Error>")
    public init<T: Error>(resolvers: (_ fulfill: (T) -> Void, _ reject: (Error) -> Void) throws -> Void) { fatalError() }

    @available(*, unavailable, message: "cannot instantiate Promise<Error>")
    public class func pending<T: Error>() -> (promise: Promise, fulfill: (T) -> Void, reject: (Error) -> Void) { fatalError() }

//MARK: disallow returning `Error`

    @available (*, unavailable, message: "instead of returning the error; throw")
    public func then<U: Error>(on: DispatchQueue = .default, execute body: (T) throws -> U) -> Promise<U> { fatalError() }

    @available (*, unavailable, message: "instead of returning the error; throw")
    public func recover<T: Error>(on: DispatchQueue = .default, execute body: (Error) throws -> T) -> Promise { fatalError() }

//MARK: disallow returning `Promise?`

    @available(*, unavailable, message: "unwrap the promise")
    public func then<U>(on: DispatchQueue = .default, execute body: (T) throws -> Promise<U>?) -> Promise<U> { fatalError() }

    @available(*, unavailable, message: "unwrap the promise")
    public func recover(on: DispatchQueue = .default, execute body: (Error) throws -> Promise?) -> Promise { fatalError() }
}

extension Promise: CustomStringConvertible {
    public var description: String {
        return "Promise: \(state)"
    }
}

/**
 Judicious use of `firstly` *may* make chains more readable.

 Compare:

     NSURLSession.GET(url1).then {
         NSURLSession.GET(url2)
     }.then {
         NSURLSession.GET(url3)
     }

 With:

     firstly {
         NSURLSession.GET(url1)
     }.then {
         NSURLSession.GET(url2)
     }.then {
         NSURLSession.GET(url3)
     }
 */
public func firstly<T>(execute body: () throws -> Promise<T>) -> Promise<T> {
    do {
        return try body()
    } catch {
        return Promise(error: error)
    }
}

@available(*, unavailable, message: "instead of returning the error; throw")
public func firstly<T: Error>(execute body: () throws -> T) -> Promise<T> { fatalError() }

@available(*, unavailable, message: "use DispatchQueue.promise")
public func firstly<T>(on: DispatchQueue, execute body: () throws -> Promise<T>) -> Promise<T> { fatalError() }

@available(*, deprecated: 4.0, renamed: "DispatchQueue.promise")
public func dispatch_promise<T>(_ on: DispatchQueue, _ body: @escaping () throws -> T) -> Promise<T> {
    return Promise(value: ()).then(on: on, execute: body)
}


/**
 The underlying resolved state of a promise.
 - remark: Same as `Resolution<T>` but without the associated `ErrorConsumptionToken`.
*/
public enum Result<T> {
    /// Fulfillment
    case fulfilled(T)
    /// Rejection
    case rejected(Error)

    init(_ resolution: Resolution<T>) {
        switch resolution {
        case .fulfilled(let value):
            self = .fulfilled(value)
        case .rejected(let error, _):
            self = .rejected(error)
        }
    }
}


public class PMKJoint<T> {
    fileprivate var resolve: ((Resolution<T>) -> Void)!
}

extension Promise {
    public final class func joint() -> (Promise<T>, (PMKJoint<T>)) {
        let pipe = PMKJoint<T>()
        let promise = Promise(sealant: { pipe.resolve = $0 })
        return (promise, pipe)
    }

    public func join(_ joint: PMKJoint<T>) {
        state.pipe(joint.resolve)
    }
}
