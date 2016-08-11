#import "AnyPromise.h"
#import "AnyPromise+Private.h"
@import Foundation.NSDictionary;
@import Foundation.NSError;
@import Foundation.NSProgress;
@import Foundation.NSNull;
#import <libkern/OSAtomic.h>
#import "Umbrella.h"

// NSProgress resources:
//  * https://robots.thoughtbot.com/asynchronous-nsprogress
//  * http://oleb.net/blog/2014/03/nsprogress/
// NSProgress! Beware!
//  * https://github.com/AFNetworking/AFNetworking/issues/2261

AnyPromise *PMKWhen(id promises) {
    if (promises == nil)
        return [AnyPromise promiseWithValue:[NSError errorWithDomain:PMKErrorDomain code:PMKInvalidUsageError userInfo:@{NSLocalizedDescriptionKey: @"PMKWhen(nil)"}]];

    if ([promises isKindOfClass:[NSArray class]] || [promises isKindOfClass:[NSDictionary class]]) {
        if ([promises count] == 0)
            return [AnyPromise promiseWithValue:promises];
    } else if ([promises isKindOfClass:[AnyPromise class]]) {
        promises = @[promises];
    } else {
        return [AnyPromise promiseWithValue:promises];
    }

#ifndef PMKDisableProgress
    NSProgress *progress = [NSProgress progressWithTotalUnitCount:[promises count]];
    progress.pausable = NO;
    progress.cancellable = NO;
#else
    struct PMKProgress {
        int completedUnitCount;
        int totalUnitCount;
    };
    __block struct PMKProgress progress;
#endif

    __block int32_t countdown = (int32_t)[promises count];
    BOOL const isdict = [promises isKindOfClass:[NSDictionary class]];

    return [AnyPromise promiseWithResolverBlock:^(PMKResolver resolve) {
        NSInteger index = 0;

        for (__strong id key in promises) {
            AnyPromise *promise = isdict ? promises[key] : key;
            if (!isdict) key = @(index);

            if (![promise isKindOfClass:[AnyPromise class]])
                promise = [AnyPromise promiseWithValue:promise];

            [promise pipe:^(id value){
                if (progress.fractionCompleted >= 1)
                    return;

                if (IsError(value)) {
                    progress.completedUnitCount = progress.totalUnitCount;
                    resolve(NSErrorSupplement(value, @{PMKFailingPromiseIndexKey: key}));
                }
                else if (OSAtomicDecrement32(&countdown) == 0) {
                    progress.completedUnitCount = progress.totalUnitCount;

                    id results;
                    if (isdict) {
                        results = [NSMutableDictionary new];
                        for (id key in promises) {
                            id promise = promises[key];
                            results[key] = IsPromise(promise) ? ((AnyPromise *)promise).value : promise;
                        }
                    } else {
                        results = [NSMutableArray new];
                        for (AnyPromise *promise in promises) {
                            id value = IsPromise(promise) ? (promise.value ?: [NSNull null]) : promise;
                            [results addObject:value];
                        }
                    }
                    resolve(results);
                } else {
                    progress.completedUnitCount++;
                }
            }];
        }
    }];
}
