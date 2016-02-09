#import <PromiseKit/PromiseKit.h>
#import "UIAlertView+AnyPromise.h"


@interface PMKAlertViewDelegate : NSObject <UIAlertViewDelegate> {
@public
    PMKResolver resolve;
    id retainCycle;
}
@end


@implementation PMKAlertViewDelegate

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex {
    if (buttonIndex != alertView.cancelButtonIndex) {
        resolve(PMKManifold(@(buttonIndex), alertView));
    } else {
        resolve([NSError cancelledError]);
    }
    retainCycle = nil;
}

@end


@implementation UIAlertView (PromiseKit)

- (AnyPromise *)promise {
    PMKAlertViewDelegate *d = [PMKAlertViewDelegate new];
    d->retainCycle = self.delegate = d;
    [self show];

    if (self.numberOfButtons == 1 && self.cancelButtonIndex == 0) {
        NSLog(@"PromiseKit: An alert view is being promised with a single button that is set as the cancelButtonIndex. The promise *will* be cancelled which may result in unexpected behavior. See http://promisekit.org/PromiseKit-2.0-Released/ for cancellation documentation.");
    }

    return [[AnyPromise alloc] initWithResolver:&d->resolve];
}

@end
