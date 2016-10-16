import UIKit
#if !COCOAPODS
import PromiseKit
#endif

//TODO tests
//TODO NSCoding

/**
 A “promisable” UIAlertController.

 UIAlertController is not a suitable API for an extension; it has closure
 handlers on its main API for each button and an extension would have to
 either replace all these when the controller is presented or force you to
 use an extended addAction method, which would be easy to forget part of
 the time. Hence we provide a facade pattern that can be promised.

    let alert = PMKAlertController("OHAI")
    let sup = alert.addActionWithTitle("SUP")
    let bye = alert.addActionWithTitle("BYE")
    promiseViewController(alert).then { action in
        switch action {
        case is sup:
            //…
        case is bye:
            //…
        }
    }
*/
public class PMKAlertController {
    /// The title of the alert.
    public var title: String? { return UIAlertController.title }
    /// Descriptive text that provides more details about the reason for the alert.
    public var message: String? { return UIAlertController.message }
    /// The style of the alert controller.
    public var preferredStyle: UIAlertControllerStyle { return UIAlertController.preferredStyle }
    /// The actions that the user can take in response to the alert or action sheet.
    public var actions: [UIAlertAction] { return UIAlertController.actions }
    /// The array of text fields displayed by the alert.
    public var textFields: [UITextField]? { return UIAlertController.textFields }

#if !os(tvOS)
    /// The nearest popover presentation controller that is managing the current view controller.
    public var popoverPresentationController: UIPopoverPresentationController? { return UIAlertController.popoverPresentationController }
#endif

    /// Creates and returns a view controller for displaying an alert to the user.
    public required init(title: String?, message: String?  = nil, preferredStyle: UIAlertControllerStyle = .alert) {
        UIAlertController = UIKit.UIAlertController(title: title, message: message, preferredStyle: preferredStyle)
    }

    /// Attaches an action title to the alert or action sheet.
    public func addActionWithTitle(title: String, style: UIAlertActionStyle = .default) -> UIAlertAction {
        let action = UIAlertAction(title: title, style: style) { action in
            if style != .cancel {
                self.fulfill(action)
            } else {
                self.reject(Error.cancelled)
            }
        }
        UIAlertController.addAction(action)
        return action
    }

    /// Adds a text field to an alert.
    public func addTextFieldWithConfigurationHandler(configurationHandler: ((UITextField) -> Void)?) {
        UIAlertController.addTextField(configurationHandler: configurationHandler)
    }

    fileprivate let UIAlertController: UIKit.UIAlertController
    fileprivate let (promise, fulfill, reject) = Promise<UIAlertAction>.pending()
    fileprivate var retainCycle: PMKAlertController?

    /// Errors that represent PMKAlertController failures
    public enum Error: CancellableError {
        /// The user cancelled the PMKAlertController.
        case cancelled

        /// - Returns: true
        public var isCancelled: Bool {
            return self == .cancelled
        }
    }
}

extension UIViewController {
    /// Presents the PMKAlertController, resolving with the user action.
    public func promise(_ vc: PMKAlertController, animated: Bool = true, completion: (() -> Void)? = nil) -> Promise<UIAlertAction> {
        vc.retainCycle = vc
        present(vc.UIAlertController, animated: animated, completion: completion)
        _ = vc.promise.always { _ -> Void in
            vc.retainCycle = nil
        }
        return vc.promise
    }
}
