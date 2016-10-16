import class Dispatch.DispatchQueue

extension Promise {
    /**
     The provided closure executes once this promise resolves.

     - Parameter on: The queue on which the provided closure executes.
     - Parameter body: The closure that is executed when this promise fulfills.
     - Returns: A new promise that resolves when the `AnyPromise` returned from the provided closure resolves. For example:

           NSURLSession.GET(url).then { (data: NSData) -> AnyPromise in
               //…
               return SCNetworkReachability()
           }.then { _ in
               //…
           }
     */
    public func then(on q: DispatchQueue = .default, execute body: @escaping (T) throws -> AnyPromise) -> Promise<Any?> {
        return Promise<Any?>(sealant: { resolve in
            state.then(on: q, else: resolve) { value in
                try body(value).state.pipe(resolve)
            }
        })
    }

    @available(*, unavailable, message: "unwrap the promise")
    public func then(on: DispatchQueue = .default, execute body: (T) throws -> AnyPromise?) -> Promise<AnyObject?> { fatalError() }
}

/**
 `firstly` can make chains more readable.
*/
public func firstly(execute body: () throws -> AnyPromise) -> Promise<Any?> {
    return Promise(sealant: { resolve in
        do {
            try body().state.pipe(resolve)
        } catch {
            resolve(Resolution(error))
        }
    })
}
