#import <PromiseKit/PromiseKit.h>
#import "UIActionSheet+AnyPromise.h"


@interface PMKActionSheetDelegate : NSObject <UIActionSheetDelegate> {
@public
    id retainCycle;
    PMKResolver resolve;
}
@end


@implementation PMKActionSheetDelegate

- (void)actionSheet:(UIActionSheet *)actionSheet didDismissWithButtonIndex:(NSInteger)buttonIndex {
    if (buttonIndex == actionSheet.cancelButtonIndex) {
        resolve([NSError cancelledError]);
    } else {
        resolve(PMKManifold(@(buttonIndex), actionSheet));
    }
    retainCycle = nil;
}

@end


@implementation UIActionSheet (PromiseKit)

- (AnyPromise *)promiseInView:(UIView *)view {
    PMKActionSheetDelegate *d = [PMKActionSheetDelegate new];
    d->retainCycle = self.delegate = d;
    [self showInView:view];

    if (self.numberOfButtons == 1 && self.cancelButtonIndex == 0) {
        NSLog(@"PromiseKit: An action sheet is being promised with a single button that is set as the cancelButtonIndex. The promise *will* be cancelled which may result in unexpected behavior. See http://promisekit.org/PromiseKit-2.0-Released/ for cancellation documentation.");
    }

    return [[AnyPromise alloc] initWithResolver:&d->resolve];
}

@end
