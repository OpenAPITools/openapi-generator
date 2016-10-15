import class Dispatch.DispatchQueue
import class Foundation.Thread

/**
 `zalgo` causes your handlers to be executed as soon as their promise resolves.

 Usually all handlers are dispatched to a queue (the main queue by default); the `on:` parameter of `then` configures this. Its default value is `DispatchQueue.main`.

 - Important: `zalgo` is dangerous.

    Compare:

       var x = 0
       foo.then {
           print(x)  // => 1
       }
       x++

    With:

       var x = 0
       foo.then(on: zalgo) {
           print(x)  // => 0 or 1
       }
       x++
 
    In the latter case the value of `x` may be `0` or `1` depending on whether `foo` is resolved. This is a race-condition that is easily avoided by not using `zalgo`.

 - Important: you cannot control the queue that your handler executes if using `zalgo`.

 - Note: `zalgo` is provided for libraries providing promises that have good tests that prove “Unleashing Zalgo” is safe. You can also use it in your application code in situations where performance is critical, but be careful: read the essay liked below to understand the risks.

 - SeeAlso: [Designing APIs for Asynchrony](http://blog.izs.me/post/59142742143/designing-apis-for-asynchrony)
 - SeeAlso: `waldo`
 */
public let zalgo = DispatchQueue(label: "Zalgo")

/**
 `waldo` is dangerous.

 `waldo` is `zalgo`, unless the current queue is the main thread, in which
 case we dispatch to the default background queue.

 If your block is likely to take more than a few milliseconds to execute,
 then you should use waldo: 60fps means the main thread cannot hang longer
 than 17 milliseconds: don’t contribute to UI lag.

 Conversely if your then block is trivial, use zalgo: GCD is not free and
 for whatever reason you may already be on the main thread so just do what
 you are doing quickly and pass on execution.

 It is considered good practice for asynchronous APIs to complete onto the
 main thread. Apple do not always honor this, nor do other developers.
 However, they *should*. In that respect waldo is a good choice if your
 then is going to take some time and doesn’t interact with the UI.

 Please note (again) that generally you should not use `zalgo` or `waldo`.
 The performance gains are neglible and we provide these functions only out
 of a misguided sense that library code should be as optimized as possible.
 If you use either without tests proving their correctness you may
 unwillingly introduce horrendous, near-impossible-to-trace bugs.

 - SeeAlso: `zalgo`
 */
public let waldo = DispatchQueue(label: "Waldo")


@inline(__always) func contain_zalgo(_ q: DispatchQueue, body: @escaping () -> Void) {
    if q === zalgo || q === waldo && !Thread.isMainThread {
        body()
    } else {
        q.async(execute: body)
    }
}

@inline(__always) func contain_zalgo<T>(_ q: DispatchQueue, rejecter reject: @escaping (Resolution<T>) -> Void, block: @escaping () throws -> Void) {
    contain_zalgo(q) {
        do { try block() } catch { reject(Resolution(error)) }
    }
}
