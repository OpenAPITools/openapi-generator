import Dispatch

/**
 ```
 DispatchQueue.global().promise {
     try md5(input)
 }.then { md5 in
     //…
 }
 ```

 - Parameter body: The closure that resolves this promise.
 - Returns: A new promise resolved by the result of the provided closure.
*/
extension DispatchQueue {
    /**
     - SeeAlso: `dispatch_promise()`
     - SeeAlso: `dispatch_promise_on()`
     */
    public final func promise<T>(group: DispatchGroup? = nil, qos: DispatchQoS = .default, flags: DispatchWorkItemFlags = [], execute body: @escaping () throws -> T) -> Promise<T> {

        return Promise(sealant: { resolve in
            async(group: group, qos: qos, flags: flags) {
                do {
                    resolve(.fulfilled(try body()))
                } catch {
                    resolve(Resolution(error))
                }
            }
        })
    }

    /// Unavailable due to Swift compiler issues
    @available(*, unavailable)
    public final func promise<T>(group: DispatchGroup? = nil, qos: DispatchQoS = .default, flags: DispatchWorkItemFlags = [], execute body: () throws -> Promise<T>) -> Promise<T> { fatalError() }

    /**
     - SeeAlso: `PMKDefaultDispatchQueue()`
     - SeeAlso: `PMKSetDefaultDispatchQueue()`
     */
    class public final var `default`: DispatchQueue {
        get {
            return __PMKDefaultDispatchQueue()
        }
        set {
            __PMKSetDefaultDispatchQueue(newValue)
        }
    }
}
