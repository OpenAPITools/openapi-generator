@import Foundation.NSDictionary;
#import "AnyPromise+Private.h"
@import Foundation.NSProgress;
#import <libkern/OSAtomic.h>
@import Foundation.NSError;
@import Foundation.NSNull;
#import "PromiseKit.h"

// NSProgress resources:
//  * https://robots.thoughtbot.com/asynchronous-nsprogress
//  * http://oleb.net/blog/2014/03/nsprogress/
// NSProgress! Beware!
//  * https://github.com/AFNetworking/AFNetworking/issues/2261

/**
 Wait for all promises in a set to resolve.

 @note If *any* of the provided promises reject, the returned promise is immediately rejected with that error.
 @warning In the event of rejection the other promises will continue to resolve and, as per any other promise, will either fulfill or reject. This is the right pattern for `getter` style asynchronous tasks, but often for `setter` tasks (eg. storing data on a server), you most likely will need to wait on all tasks and then act based on which have succeeded and which have failed, in such situations use `when(resolved:)`.
 @param promises The promises upon which to wait before the returned promise resolves.
 @note PMKWhen provides NSProgress.
 @return A new promise that resolves when all the provided promises fulfill or one of the provided promises rejects.
*/
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
    NSProgress *progress = [NSProgress progressWithTotalUnitCount:(int64_t)[promises count]];
    progress.pausable = NO;
    progress.cancellable = NO;
#else
    struct PMKProgress {
        int completedUnitCount;
        int totalUnitCount;
        double fractionCompleted;
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

            [promise __pipe:^(id value){
                if (progress.fractionCompleted >= 1)
                    return;

                if (IsError(value)) {
                    progress.completedUnitCount = progress.totalUnitCount;

                    NSMutableDictionary *userInfo = [NSMutableDictionary dictionaryWithDictionary:[value userInfo] ?: @{}];
                    userInfo[PMKFailingPromiseIndexKey] = key;
                    [userInfo setObject:value forKey:NSUnderlyingErrorKey];
                    id err = [[NSError alloc] initWithDomain:[value domain] code:[value code] userInfo:userInfo];
                    resolve(err);
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
