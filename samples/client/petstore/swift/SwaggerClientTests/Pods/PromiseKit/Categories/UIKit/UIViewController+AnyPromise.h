#import <PromiseKit/AnyPromise.h>
#import <UIKit/UIViewController.h>

/**
 To import the `UIViewController` category:

    use_frameworks!
    pod "PromiseKit/UIKit"

 Or `UIKit` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"
 
 And then in your sources:

    #import <PromiseKit/PromiseKit.h>
*/
@interface UIViewController (PromiseKit)

/**
 Presents a view controller modally.

 If the view controller is one of the following:

  - MFMailComposeViewController
  - MFMessageComposeViewController
  - UIImagePickerController
  - SLComposeViewController

 Then PromiseKit presents the view controller returning a promise that is
 resolved as per the documentation for those classes. Eg. if you present a
 `UIImagePickerController` the view controller will be presented for you
 and the returned promise will resolve with the media the user selected.

    [self promiseViewController:[MFMailComposeViewController new] animated:YES completion:nil].then(^{
        //…
    });

 Otherwise PromiseKit expects your view controller to implement a
 `promise` property. This promise will be returned from this method and
 presentation and dismissal of the presented view controller will be
 managed for you.
 
    @interface MyViewController: UIViewController
    @property (readonly) AnyPromise *promise;
    @end

    @implementation MyViewController {
        PMKResolver resolve;
    }
 
    - (void)viewDidLoad {
        _promise = [AnyPromise promiseWithResolver:&resolve];
    }
 
    - (void)later {
        resolve(@"some fulfilled value");
    }

    @end

 

    [self promiseViewController:[MyViewController new] aniamted:YES completion:nil].then(^(id value){
        // value == @"some fulfilled value"
    });

 @return A promise that can be resolved by the presented view controller.
*/
- (AnyPromise *)promiseViewController:(UIViewController *)vc animated:(BOOL)animated completion:(void (^)(void))block;

@end



@interface UIViewController (PMKUnavailable)

#define PMKRationale \
    "The promiseViewController system has been rennovated: the fullfil and " \
    "reject category methods have been removed due to runtime safety " \
    "concerns and instead you should implement a -promise property on your " \
    "view controller subclass. @see promiseViewController:animated:completion:"

- (void)fulfill:(id)value __attribute__((unavailable(PMKRationale)));
- (void)reject:(NSError *)value __attribute__((unavailable(PMKRationale)));

#undef PMKRationale

@end
