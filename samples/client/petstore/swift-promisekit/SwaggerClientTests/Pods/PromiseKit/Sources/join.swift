import Dispatch

/**
 Waits on all provided promises.

 `when` rejects as soon as one of the provided promises rejects. `join` waits on all provided promises, then rejects if any of those promises rejected, otherwise it fulfills with values from the provided promises.

     join(promise1, promise2, promise3).then { results in
         //…
     }.error { error in
         switch error {
         case Error.Join(let promises):
             //…
         }
     }

 - Returns: A new promise that resolves once all the provided promises resolve.
*/
public func join<T>(promises: Promise<T>...) -> Promise<[T]> {
    return join(promises)
}

public func join<T>(promises: [Promise<T>]) -> Promise<[T]> {
    guard !promises.isEmpty else { return Promise<[T]>([]) }
  
    var countdown = promises.count
    let barrier = dispatch_queue_create("org.promisekit.barrier.join", DISPATCH_QUEUE_CONCURRENT)
    var rejected = false

    return Promise { fulfill, reject in
        for promise in promises {
            promise.pipe { resolution in
                dispatch_barrier_sync(barrier) {
                    if case .Rejected(_, let token) = resolution {
                        token.consumed = true  // the parent Error.Join consumes all
                        rejected = true
                    }
                    countdown -= 1
                    if countdown == 0 {
                        if rejected {
                            reject(Error.Join(promises))
                        } else {
                            fulfill(promises.map{ $0.value! })
                        }
                    }
                }
            }
        }
    }
}
