/**
 Resolves with the first resolving promise from a set of promises.

 ```
 race(promise1, promise2, promise3).then { winner in
     //…
 }
 ```

 - Returns: A new promise that resolves when the first promise in the provided promises resolves.
 - Warning: If any of the provided promises reject, the returned promise is rejected.
 - Warning: aborts if the array is empty.
*/
public func race<T>(promises: [Promise<T>]) -> Promise<T> {
    guard promises.count > 0 else {
        fatalError("Cannot race with an empty array of promises")
    }
    return _race(promises: promises)
}

/**
 Resolves with the first resolving promise from a set of promises.

 ```
 race(promise1, promise2, promise3).then { winner in
     //…
 }
 ```

 - Returns: A new promise that resolves when the first promise in the provided promises resolves.
 - Warning: If any of the provided promises reject, the returned promise is rejected.
 - Warning: aborts if the array is empty.
*/
public func race<T>(_ promises: Promise<T>...) -> Promise<T> {
    return _race(promises: promises)
}

private func _race<T>(promises: [Promise<T>]) -> Promise<T> {
    return Promise(sealant: { resolve in
        for promise in promises {
            promise.state.pipe(resolve)
        }
    })
}
