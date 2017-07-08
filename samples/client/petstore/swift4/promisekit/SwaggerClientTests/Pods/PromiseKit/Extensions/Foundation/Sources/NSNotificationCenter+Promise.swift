import Foundation.NSNotification
#if !COCOAPODS
import PromiseKit
#endif

/**
 To import the `NSNotificationCenter` category:

    use_frameworks!
    pod "PromiseKit/Foundation"

 Or `NSNotificationCenter` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    import PromiseKit
*/
extension NotificationCenter {
    /// Observe the named notification once
    public func observe(once name: Notification.Name, object: Any? = nil) -> NotificationPromise {
        let (promise, fulfill) = NotificationPromise.go()
        let id = addObserver(forName: name, object: object, queue: nil, using: fulfill)
        _ = promise.always { self.removeObserver(id) }
        return promise
    }
}

/// The promise returned by `NotificationCenter.observe(once:)`
public class NotificationPromise: Promise<[AnyHashable: Any]> {
    private let pending = Promise<Notification>.pending()

    public func asNotification() -> Promise<Notification> {
        return pending.promise
    }

    fileprivate class func go() -> (NotificationPromise, (Notification) -> Void) {
        let (p, fulfill, _) = NotificationPromise.pending()
        let promise = p as! NotificationPromise
        _ = promise.pending.promise.then { fulfill($0.userInfo ?? [:]) }
        return (promise, promise.pending.fulfill)
    }
}
