import Foundation

/**
 AnyPromise is an Objective-C compatible promise.
*/
@objc(AnyPromise) public class AnyPromise: NSObject {
    let state: State<Any?>

    /**
     - Returns: A new `AnyPromise` bound to a `Promise<Any>`.
    */
    required public init(_ bridge: Promise<Any?>) {
        state = bridge.state
    }

    /// hack to ensure Swift picks the right initializer for each of the below
    private init(force: Promise<Any?>) {
        state = force.state
    }

    /**
     - Returns: A new `AnyPromise` bound to a `Promise<T>`.
    */
    public convenience init<T>(_ bridge: Promise<T?>) {
        self.init(force: bridge.then(on: zalgo) { $0 })
    }

    /**
     - Returns: A new `AnyPromise` bound to a `Promise<T>`.
    */
    convenience public init<T>(_ bridge: Promise<T>) {
        self.init(force: bridge.then(on: zalgo) { $0 })
    }

    /**
     - Returns: A new `AnyPromise` bound to a `Promise<Void>`.
     - Note: A “void” `AnyPromise` has a value of `nil`.
    */
    convenience public init(_ bridge: Promise<Void>) {
        self.init(force: bridge.then(on: zalgo) { nil })
    }

    /**
     Bridge an AnyPromise to a Promise<Any>
     - Note: AnyPromises fulfilled with `PMKManifold` lose all but the first fulfillment object.
     - Remark: Could not make this an initializer of `Promise` due to generics issues.
     */
    public func asPromise() -> Promise<Any?> {
        return Promise(sealant: { resolve in
            state.pipe { resolution in
                switch resolution {
                case .rejected:
                    resolve(resolution)
                case .fulfilled:
                    let obj = (self as AnyObject).value(forKey: "value")
                    resolve(.fulfilled(obj))
                }
            }
        })
    }

    /// - See: `Promise.then()`
    public func then<T>(on q: DispatchQueue = .default, execute body: @escaping (Any?) throws -> T) -> Promise<T> {
        return asPromise().then(on: q, execute: body)
    }

    /// - See: `Promise.then()`
    public func then(on q: DispatchQueue = .default, execute body: @escaping (Any?) throws -> AnyPromise) -> Promise<Any?> {
        return asPromise().then(on: q, execute: body)
    }

    /// - See: `Promise.then()`
    public func then<T>(on q: DispatchQueue = .default, execute body: @escaping (Any?) throws -> Promise<T>) -> Promise<T> {
        return asPromise().then(on: q, execute: body)
    }

    /// - See: `Promise.always()`
    public func always(on q: DispatchQueue = .default, execute body: @escaping () -> Void) -> Promise<Any?> {
        return asPromise().always(execute: body)
    }

    /// - See: `Promise.tap()`
    public func tap(on q: DispatchQueue = .default, execute body: @escaping (Result<Any?>) -> Void) -> Promise<Any?> {
        return asPromise().tap(execute: body)
    }

    /// - See: `Promise.recover()`
    public func recover(on q: DispatchQueue = .default, policy: CatchPolicy = .allErrorsExceptCancellation, execute body: @escaping (Error) throws -> Promise<Any?>) -> Promise<Any?> {
        return asPromise().recover(on: q, policy: policy, execute: body)
    }

    /// - See: `Promise.recover()`
    public func recover(on q: DispatchQueue = .default, policy: CatchPolicy = .allErrorsExceptCancellation, execute body: @escaping (Error) throws -> Any?) -> Promise<Any?> {
        return asPromise().recover(on: q, policy: policy, execute: body)
    }

    /// - See: `Promise.catch()`
    public func `catch`(on q: DispatchQueue = .default, policy: CatchPolicy = .allErrorsExceptCancellation, execute body: @escaping (Error) -> Void) {
        state.catch(on: q, policy: policy, else: { _ in }, execute: body)
    }

//MARK: ObjC methods

    /**
     A promise starts pending and eventually resolves.
     - Returns: `true` if the promise has not yet resolved.
     */
    @objc public var pending: Bool {
        return state.get() == nil
    }

    /**
     A promise starts pending and eventually resolves.
     - Returns: `true` if the promise has resolved.
     */
    @objc public var resolved: Bool {
        return !pending
    }

    /**
     The value of the asynchronous task this promise represents.

     A promise has `nil` value if the asynchronous task it represents has not finished. If the value is `nil` the promise is still `pending`.

     - Warning: *Note* Our Swift variant’s value property returns nil if the promise is rejected where AnyPromise will return the error object. This fits with the pattern where AnyPromise is not strictly typed and is more dynamic, but you should be aware of the distinction.
     
     - Note: If the AnyPromise was fulfilled with a `PMKManifold`, returns only the first fulfillment object.

     - Returns If `resolved`, the object that was used to resolve this promise; if `pending`, nil.
     */
    @objc private var __value: Any? {
        switch state.get() {
        case nil:
            return nil
        case .rejected(let error, _)?:
            return error
        case .fulfilled(let obj)?:
            return obj
        }
    }

    /**
     Creates a resolved promise.

     When developing your own promise systems, it is ocassionally useful to be able to return an already resolved promise.

     - Parameter value: The value with which to resolve this promise. Passing an `NSError` will cause the promise to be rejected, passing an AnyPromise will return a new AnyPromise bound to that promise, otherwise the promise will be fulfilled with the value passed.

     - Returns: A resolved promise.
     */
    @objc public class func promiseWithValue(_ value: Any?) -> AnyPromise {
        let state: State<Any?>
        switch value {
        case let promise as AnyPromise:
            state = promise.state
        case let err as Error:
            state = SealedState(resolution: Resolution(err))
        default:
            state = SealedState(resolution: .fulfilled(value))
        }
        return AnyPromise(state: state)
    }

    private init(state: State<Any?>) {
        self.state = state
    }

    /**
     Create a new promise that resolves with the provided block.

     Use this method when wrapping asynchronous code that does *not* use promises so that this code can be used in promise chains.

     If `resolve` is called with an `NSError` object, the promise is rejected, otherwise the promise is fulfilled.

     Don’t use this method if you already have promises! Instead, just return your promise.

     Should you need to fulfill a promise but have no sensical value to use: your promise is a `void` promise: fulfill with `nil`.

     The block you pass is executed immediately on the calling thread.

     - Parameter block: The provided block is immediately executed, inside the block call `resolve` to resolve this promise and cause any attached handlers to execute. If you are wrapping a delegate-based system, we recommend instead to use: initWithResolver:

     - Returns: A new promise.
     - Warning: Resolving a promise with `nil` fulfills it.
     - SeeAlso: http://promisekit.org/sealing-your-own-promises/
     - SeeAlso: http://promisekit.org/wrapping-delegation/
     */
    @objc public class func promiseWithResolverBlock(_ body: (@escaping (Any?) -> Void) -> Void) -> AnyPromise {
        return AnyPromise(sealant: { resolve in
            body { obj in
                makeHandler({ _ in obj }, resolve)(obj)
            }
        })
    }

    private init(sealant: (@escaping (Resolution<Any?>) -> Void) -> Void) {
        var resolve: ((Resolution<Any?>) -> Void)!
        state = UnsealedState(resolver: &resolve)
        sealant(resolve)
    }

    @objc func __thenOn(_ q: DispatchQueue, execute body: @escaping (Any?) -> Any?) -> AnyPromise {
        return AnyPromise(sealant: { resolve in
            state.then(on: q, else: resolve, execute: makeHandler(body, resolve))
        })
    }

    @objc func __catchWithPolicy(_ policy: CatchPolicy, execute body: @escaping (Any?) -> Any?) -> AnyPromise {
        return AnyPromise(sealant: { resolve in
            state.catch(on: .default, policy: policy, else: resolve) { err in
                makeHandler(body, resolve)(err as NSError)
            }
        })
    }

    @objc func __alwaysOn(_ q: DispatchQueue, execute body: @escaping () -> Void) -> AnyPromise {
        return AnyPromise(sealant: { resolve in
            state.always(on: q) { resolution in
                body()
                resolve(resolution)
            }
        })
    }

    /**
     Convert an `AnyPromise` to `Promise<T>`.

         anyPromise.toPromise(T).then { (t: T) -> U in ... }
     
     - Returns: A `Promise<T>` with the requested type.
     - Throws: `CastingError.CastingAnyPromiseFailed(T)` if self's value cannot be downcasted to the given type.
     */
    public func asPromise<T>(type: T.Type) -> Promise<T> {
        return self.then(on: zalgo) { (value: Any?) -> T in
            if let value = value as? T {
                return value
            }
            throw PMKError.castError(type)
        }
    }

    /// used by PMKWhen and PMKJoin
    @objc func __pipe(_ body: @escaping (Any?) -> Void) {
        state.pipe { resolution in
            switch resolution {
            case .rejected(let error, let token):
                token.consumed = true  // when and join will create a new parent error that is unconsumed
                body(error as Error)
            case .fulfilled(let value):
                body(value)
            }
        }
    }
}


extension AnyPromise {
    override public var description: String {
        return "AnyPromise: \(state)"
    }
}

private func makeHandler(_ body: @escaping (Any?) -> Any?, _ resolve: @escaping (Resolution<Any?>) -> Void) -> (Any?) -> Void {
    return { obj in
        let obj = body(obj)
        switch obj {
        case let err as Error:
            resolve(Resolution(err))
        case let promise as AnyPromise:
            promise.state.pipe(resolve)
        default:
            resolve(.fulfilled(obj))
        }
    }
}
