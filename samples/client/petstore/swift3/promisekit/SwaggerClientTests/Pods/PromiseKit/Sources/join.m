@import Foundation.NSDictionary;
#import "AnyPromise+Private.h"
#import <libkern/OSAtomic.h>
@import Foundation.NSError;
@import Foundation.NSNull;
#import "PromiseKit.h"
#import <stdatomic.h>

/**
 Waits on all provided promises.

 `PMKWhen` rejects as soon as one of the provided promises rejects. `PMKJoin` waits on all provided promises, then rejects if any of those promises rejects, otherwise it fulfills with values from the provided promises.

 - Returns: A new promise that resolves once all the provided promises resolve.
*/
AnyPromise *PMKJoin(NSArray *promises) {
    if (promises == nil)
        return [AnyPromise promiseWithValue:[NSError errorWithDomain:PMKErrorDomain code:PMKInvalidUsageError userInfo:@{NSLocalizedDescriptionKey: @"PMKJoin(nil)"}]];

    if (promises.count == 0)
        return [AnyPromise promiseWithValue:promises];

    return [AnyPromise promiseWithResolverBlock:^(PMKResolver resolve) {
        NSPointerArray *results = NSPointerArrayMake(promises.count);
        __block atomic_int countdown = promises.count;
        __block BOOL rejected = NO;

        [promises enumerateObjectsUsingBlock:^(AnyPromise *promise, NSUInteger ii, BOOL *stop) {
            [promise __pipe:^(id value) {

                if (IsError(value)) {
                    rejected = YES;
                }

                //FIXME surely this isn't thread safe on multiple cores?
                [results replacePointerAtIndex:ii withPointer:(__bridge void *)(value ?: [NSNull null])];

                atomic_fetch_sub_explicit(&countdown, 1, memory_order_relaxed);

                if (countdown == 0) {
                    if (!rejected) {
                        resolve(results.allObjects);
                    } else {
                        id userInfo = @{PMKJoinPromisesKey: promises};
                        id err = [NSError errorWithDomain:PMKErrorDomain code:PMKJoinError userInfo:userInfo];
                        resolve(err);
                    }
                }
            }];

            (void) stop;
        }];
    }];
}
