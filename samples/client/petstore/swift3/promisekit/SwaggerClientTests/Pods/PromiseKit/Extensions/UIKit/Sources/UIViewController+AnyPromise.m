#import <UIKit/UIImagePickerController.h>
#import <UIKit/UINavigationController.h>
#import "UIViewController+AnyPromise.h"
#import <PromiseKit/PromiseKit.h>

#if TARGET_OS_TV
#define UIImagePickerControllerDelegate UINavigationControllerDelegate
#endif


@interface PMKGenericDelegate : NSObject <UIImagePickerControllerDelegate, UINavigationControllerDelegate> {
@public
    PMKResolver resolve;
}
+ (instancetype)delegateWithPromise:(AnyPromise **)promise;
@end



@implementation UIViewController (PromiseKit)

- (AnyPromise *)promiseViewController:(UIViewController *)vc animated:(BOOL)animated completion:(void (^)(void))block {
    __kindof UIViewController *vc2present = vc;
    AnyPromise *promise = nil;

    if ([vc isKindOfClass:NSClassFromString(@"MFMailComposeViewController")]) {
        PMKGenericDelegate *delegate = [PMKGenericDelegate delegateWithPromise:&promise];
        [vc setValue:delegate forKey:@"mailComposeDelegate"];
    }
    else if ([vc isKindOfClass:NSClassFromString(@"MFMessageComposeViewController")]) {
        PMKGenericDelegate *delegate = [PMKGenericDelegate delegateWithPromise:&promise];
        [vc setValue:delegate forKey:@"messageComposeDelegate"];
    }
#if !TARGET_OS_TV
    else if ([vc isKindOfClass:NSClassFromString(@"UIImagePickerController")]) {
        PMKGenericDelegate *delegate = [PMKGenericDelegate delegateWithPromise:&promise];
        ((UIImagePickerController *)vc).delegate = delegate;
    }
#endif
    else if ([vc isKindOfClass:NSClassFromString(@"SLComposeViewController")]) {
        PMKResolver resolve;
        promise = [[AnyPromise alloc] initWithResolver:&resolve];
        [vc setValue:^(NSInteger result){
            if (result == 0) {
                resolve([NSError cancelledError]);
            } else {
                resolve(@(result));
            }
        } forKey:@"completionHandler"];
    }
    else if ([vc isKindOfClass:[UINavigationController class]])
        vc = [(id)vc viewControllers].firstObject;

    if (!vc) {
        id userInfo = @{NSLocalizedDescriptionKey: @"nil or effective nil passed to promiseViewController"};
        id err = [NSError errorWithDomain:PMKErrorDomain code:PMKInvalidUsageError userInfo:userInfo];
        return [AnyPromise promiseWithValue:err];
    }

    if (!promise) {
        if (![vc respondsToSelector:NSSelectorFromString(@"promise")]) {
            id userInfo = @{NSLocalizedDescriptionKey: @"ViewController is not promisable"};
            id err = [NSError errorWithDomain:PMKErrorDomain code:PMKInvalidUsageError userInfo:userInfo];
            return [AnyPromise promiseWithValue:err];
        }

        promise = [vc valueForKey:@"promise"];

        if (![promise isKindOfClass:[AnyPromise class]]) {
            id userInfo = @{NSLocalizedDescriptionKey: @"The promise property is nil or not of type AnyPromise"};
            id err = [NSError errorWithDomain:PMKErrorDomain code:PMKInvalidUsageError userInfo:userInfo];
            return [AnyPromise promiseWithValue:err];
        }
    }

    if (!promise.pending)
        return promise;

    [self presentViewController:vc2present animated:animated completion:block];

    promise.always(^{
        [vc2present.presentingViewController dismissViewControllerAnimated:animated completion:nil];
    });

    return promise;
}

@end



@implementation PMKGenericDelegate {
    id retainCycle;
}

+ (instancetype)delegateWithPromise:(AnyPromise **)promise; {
    PMKGenericDelegate *d = [PMKGenericDelegate new];
    d->retainCycle = d;
    *promise = [[AnyPromise alloc] initWithResolver:&d->resolve];
    return d;
}

- (void)mailComposeController:(id)controller didFinishWithResult:(int)result error:(NSError *)error {
    if (error != nil) {
        resolve(error);
    } else if (result == 0) {
        resolve([NSError cancelledError]);
    } else {
        resolve(@(result));
    }
    retainCycle = nil;
}

- (void)messageComposeViewController:(id)controller didFinishWithResult:(int)result {
    if (result == 2) {
        id userInfo = @{NSLocalizedDescriptionKey: @"The attempt to save or send the message was unsuccessful."};
        id error = [NSError errorWithDomain:PMKErrorDomain code:PMKOperationFailed userInfo:userInfo];
        resolve(error);
    } else {
        resolve(@(result));
    }
    retainCycle = nil;
}

#if !TARGET_OS_TV
- (void)imagePickerController:(UIImagePickerController *)picker didFinishPickingMediaWithInfo:(NSDictionary *)info {
    id img = info[UIImagePickerControllerEditedImage] ?: info[UIImagePickerControllerOriginalImage];
    resolve(PMKManifold(img, info));
    retainCycle = nil;
}

- (void)imagePickerControllerDidCancel:(UIImagePickerController *)picker {
    resolve([NSError cancelledError]);
    retainCycle = nil;
}
#endif

@end
