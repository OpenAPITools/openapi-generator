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
    public var title: String? { return UIAlertController.title }
    public var message: String? { return UIAlertController.message }
    public var preferredStyle: UIAlertControllerStyle { return UIAlertController.preferredStyle }
    public var actions: [UIAlertAction] { return UIAlertController.actions }
    public var textFields: [UITextField]? { return UIAlertController.textFields }

    public required init(title: String?, message: String?  = nil, preferredStyle: UIAlertControllerStyle = .Alert) {
        UIAlertController = UIKit.UIAlertController(title: title, message: message, preferredStyle: preferredStyle)
    }

    public func addActionWithTitle(title: String, style: UIAlertActionStyle = .Default) -> UIAlertAction {
        let action = UIAlertAction(title: title, style: style) { action in
            if style != UIAlertActionStyle.Cancel {
                self.fulfill(action)
            } else {
                self.reject(Error.Cancelled)
            }
        }
        UIAlertController.addAction(action)
        return action
    }

    public func addTextFieldWithConfigurationHandler(configurationHandler: ((UITextField) -> Void)?) {
        UIAlertController.addTextFieldWithConfigurationHandler(configurationHandler)
    }

    private let UIAlertController: UIKit.UIAlertController
    private let (promise, fulfill, reject) = Promise<UIAlertAction>.pendingPromise()
    private var retainCycle: PMKAlertController?

    public enum Error: CancellableErrorType {
        case Cancelled
      
      public var cancelled: Bool {
          return self == .Cancelled
      }
    }
}

extension UIViewController {
    public func promiseViewController(vc: PMKAlertController, animated: Bool = true, completion: (() -> Void)? = nil) -> Promise<UIAlertAction> {
        vc.retainCycle = vc
        presentViewController(vc.UIAlertController, animated: true, completion: nil)
        vc.promise.always { _ -> Void in
            vc.retainCycle = nil
        }
        return vc.promise
    }
}
