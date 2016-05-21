import Foundation.NSError

/**
 Resolves with the first resolving promise from a set of promises.

 ```
 race(promise1, promise2, promise3).then { winner in
     //…
 }
 ```

 - Returns: A new promise that resolves when the first promise in the provided promises resolves.
 - Warning: If any of the provided promises reject, the returned promise is rejected.
*/
public func race<T>(promises: Promise<T>...) -> Promise<T> {
    return try! race(promises)  // race only throws when the array param is empty, which is not possible from this
                                // variadic paramater version, so we can safely use `try!`
}

public func race<T>(promises: [Promise<T>]) throws -> Promise<T> {
    guard !promises.isEmpty else {
        let message = "Cannot race with an empty list of runners (Promises)"
        throw NSError(domain: PMKErrorDomain, code: PMKInvalidUsageError, userInfo: ["messaage": message])
    }
  
    return Promise(sealant: { resolve in
        for promise in promises {
            promise.pipe(resolve)
        }
    })
}
