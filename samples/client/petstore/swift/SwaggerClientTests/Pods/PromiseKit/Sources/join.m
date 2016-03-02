#import "AnyPromise.h"
#import "AnyPromise+Private.h"
@import Foundation.NSDictionary;
@import Foundation.NSError;
@import Foundation.NSNull;
#import <libkern/OSAtomic.h>
#import <PromiseKit/Umbrella.h>

@implementation AnyPromise (join)

AnyPromise *PMKJoin(NSArray *promises) {
    if (promises == nil)
        return [AnyPromise promiseWithValue:[NSError errorWithDomain:PMKErrorDomain code:PMKInvalidUsageError userInfo:@{NSLocalizedDescriptionKey: @"PMKJoin(nil)"}]];

    if (promises.count == 0)
        return [AnyPromise promiseWithValue:promises];

    return [AnyPromise promiseWithResolverBlock:^(PMKResolver resolve) {
        NSPointerArray *results = NSPointerArrayMake(promises.count);
        __block int32_t countdown = (int32_t)promises.count;
        __block BOOL rejected = NO;

        [promises enumerateObjectsUsingBlock:^(AnyPromise *promise, NSUInteger ii, BOOL *stop) {
            [promise pipe:^(id value) {

                if (IsError(value)) {
                    [value pmk_consume];
                    rejected = YES;
                }

                [results replacePointerAtIndex:ii withPointer:(__bridge void *)(value ?: [NSNull null])];

                if (OSAtomicDecrement32(&countdown) == 0) {
                    if (!rejected) {
                        resolve(results.allObjects);
                    } else {
                        id userInfo = @{PMKJoinPromisesKey: promises};
                        id err = [NSError errorWithDomain:PMKErrorDomain code:PMKJoinError userInfo:userInfo];
                        resolve(err);
                    }
                }
            }];
        }];
    }];
}

@end
