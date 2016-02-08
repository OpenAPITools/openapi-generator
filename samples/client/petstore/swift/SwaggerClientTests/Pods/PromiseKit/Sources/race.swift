
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
    return Promise(sealant: { resolve in
        for promise in promises {
            promise.pipe(resolve)
        }
    })
}
