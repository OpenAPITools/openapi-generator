import UIKit.UIView
#if !COCOAPODS
import PromiseKit
#endif

/**
 To import the `UIView` category:

    use_frameworks!
    pod "PromiseKit/UIKit"

 Or `UIKit` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    import PromiseKit
*/
extension UIView {
    /**
     Animate changes to one or more views using the specified duration, delay,
     options, and completion handler.
     
     - Parameter duration: The total duration of the animations, measured in
     seconds. If you specify a negative value or 0, the changes are made
     without animating them.

     - Parameter delay: The amount of time (measured in seconds) to wait before
     beginning the animations. Specify a value of 0 to begin the animations
     immediately.
     
     - Parameter options: A mask of options indicating how you want to perform the
     animations. For a list of valid constants, see UIViewAnimationOptions.

     - Parameter animations: A block object containing the changes to commit to the
     views.

     - Returns: A promise that fulfills with a boolean NSNumber indicating
     whether or not the animations actually finished.
    */
    public class func promise(animateWithDuration duration: TimeInterval, delay: TimeInterval = 0, options: UIViewAnimationOptions = [], animations: @escaping () -> Void) -> Promise<Bool> {
        return PromiseKit.wrap { animate(withDuration: duration, delay: delay, options: options, animations: animations, completion: $0) }
    }
}
