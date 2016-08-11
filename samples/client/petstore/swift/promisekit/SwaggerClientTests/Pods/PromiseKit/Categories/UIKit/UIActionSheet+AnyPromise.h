#import <PromiseKit/AnyPromise.h>
#import <UIKit/UIActionSheet.h>

/**
 To import the `UIActionSheet` category:

    use_frameworks!
    pod "PromiseKit/UIKit"

 Or `UIKit` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    #import <PromiseKit/PromiseKit.h>
*/
@interface UIActionSheet (PromiseKit)

/**
 Displays the action sheet originating from the specified view.

    UIActionSheet *sheet = [UIActionSheet new];
    sheet.title = @"OHAI";
    [sheet addButtonWithTitle:@"OK"];
    [sheet promiseInView:nil].then(^(NSNumber *dismissedButtonIndex){
        //…
    });

 @param view The view from which the action sheet originates.

 @warning *Important* If a cancelButtonIndex is set the promise will be *cancelled* if that button is pressed. Cancellation in PromiseKit has special behavior, see the relevant documentation for more details.
 
 @return A promise that fulfills with two parameters:

  1) The index (NSNumber) of the button that was tapped to dismiss the sheet.
  2) This action sheet.
*/
- (AnyPromise *)promiseInView:(UIView *)view;

@end
