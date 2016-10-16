import Foundation
#if !COCOAPODS
import PromiseKit
#endif

/**
 - Returns: A promise that resolves when the provided object deallocates
 - Important: The promise is not guarenteed to resolve immediately when the provided object is deallocated. So you cannot write code that depends on exact timing.
 */
public func after(life object: NSObject) -> Promise<Void> {
    var reaper = objc_getAssociatedObject(object, &handle) as? GrimReaper
    if reaper == nil {
        reaper = GrimReaper()
        objc_setAssociatedObject(object, &handle, reaper, .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
    }
    return reaper!.promise
}

private var handle: UInt8 = 0

private class GrimReaper: NSObject {
    deinit {
        fulfill()
    }
    let (promise, fulfill, _) = Promise<Void>.pending()
}
