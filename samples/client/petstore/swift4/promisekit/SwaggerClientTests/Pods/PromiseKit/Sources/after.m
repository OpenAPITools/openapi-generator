#import "AnyPromise.h"
@import Dispatch;
@import Foundation.NSDate;
@import Foundation.NSValue;

/// @return A promise that fulfills after the specified duration.
AnyPromise *PMKAfter(NSTimeInterval duration) {
    return [AnyPromise promiseWithResolverBlock:^(PMKResolver resolve) {
        dispatch_time_t time = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(duration * NSEC_PER_SEC));
        dispatch_after(time, dispatch_get_global_queue(0, 0), ^{
            resolve(@(duration));
        });
    }];
}
