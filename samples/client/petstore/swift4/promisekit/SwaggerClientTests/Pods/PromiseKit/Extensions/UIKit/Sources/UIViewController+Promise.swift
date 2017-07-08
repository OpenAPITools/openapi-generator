import Foundation.NSError
import UIKit
#if !COCOAPODS
import PromiseKit
#endif

/**
 To import this `UIViewController` category:

    use_frameworks!
    pod "PromiseKit/UIKit"

 Or `UIKit` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    import PromiseKit
*/
extension UIViewController {

    public enum PMKError: Error {
        case navigationControllerEmpty
        case noImageFound
        case notPromisable
        case notGenericallyPromisable
        case nilPromisable
    }

    /// Configures when a UIViewController promise resolves
    public enum FulfillmentType {
        /// The promise resolves just after the view controller has disappeared.
        case onceDisappeared
        /// The promise resolves before the view controller has disappeared.
        case beforeDismissal
    }

    /// Presents the UIViewController, resolving with the user action.
    public func promise<T>(_ vc: UIViewController, animate animationOptions: PMKAnimationOptions = [.appear, .disappear], fulfills fulfillmentType: FulfillmentType = .onceDisappeared, completion: (() -> Void)? = nil) -> Promise<T> {
        let pvc: UIViewController

        switch vc {
        case let nc as UINavigationController:
            guard let vc = nc.viewControllers.first else { return Promise(error: PMKError.navigationControllerEmpty) }
            pvc = vc
        default:
            pvc = vc
        }

        let promise: Promise<T>

        if !(pvc is Promisable) {
            promise = Promise(error: PMKError.notPromisable)
        } else if let p = pvc.value(forKeyPath: "promise") as? Promise<T> {
            promise = p
        } else if let _ = pvc.value(forKeyPath: "promise") {
            promise = Promise(error: PMKError.notGenericallyPromisable)
        } else {
            promise = Promise(error: PMKError.nilPromisable)
        }

        if !promise.isPending {
            return promise
        }

        present(vc, animated: animationOptions.contains(.appear), completion: completion)

        let (wrappingPromise, fulfill, reject) = Promise<T>.pending()

        switch fulfillmentType {
        case .onceDisappeared:
            promise.then { result in
                vc.presentingViewController?.dismiss(animated: animationOptions.contains(.disappear), completion: { fulfill(result) })
                }
                .catch(policy: .allErrors) { error in
                    vc.presentingViewController?.dismiss(animated: animationOptions.contains(.disappear), completion: { reject(error) })
            }
        case .beforeDismissal:
            promise.then { result -> Void in
                fulfill(result)
                vc.presentingViewController?.dismiss(animated: animationOptions.contains(.disappear), completion: nil)
                }
                .catch(policy: .allErrors) { error in
                    reject(error)
                    vc.presentingViewController?.dismiss(animated: animationOptions.contains(.disappear), completion: nil)
            }
        }
        
        return wrappingPromise
    }

    @available(*, deprecated: 3.4, renamed: "promise(_:animate:fulfills:completion:)")
    public func promiseViewController<T>(_ vc: UIViewController, animated: Bool = true, completion: (() -> Void)? = nil) -> Promise<T> {
        return promise(vc, animate: animated ? [.appear, .disappear] : [], completion: completion)
    }
}

/// A protocol for UIViewControllers that can be promised.
@objc(Promisable) public protocol Promisable {
    /**
     Provide a promise for promiseViewController here.

     The resulting property must be annotated with @objc.

     Obviously return a Promise<T>. There is an issue with generics and Swift and
     protocols currently so we couldn't specify that.
    */
    var promise: AnyObject! { get }
}
