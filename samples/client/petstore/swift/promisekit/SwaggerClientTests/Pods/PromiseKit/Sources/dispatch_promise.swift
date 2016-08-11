import Dispatch
import Foundation.NSError

/**
 ```
 dispatch_promise {
     try md5(input)
 }.then { md5 in
     //…
 }
 ```

 - Parameter on: The queue on which to dispatch `body`.
 - Parameter body: The closure that resolves this promise.
 - Returns: A new promise resolved by the provided closure.
*/
public func dispatch_promise<T>(on queue: dispatch_queue_t = dispatch_get_global_queue(0, 0), body: () throws -> T) -> Promise<T> {
    return Promise(sealant: { resolve in
        contain_zalgo(queue, rejecter: resolve) {
            resolve(.Fulfilled(try body()))
        }
    })
}
