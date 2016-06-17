//
//  UIView+PromiseKit_UIAnimation.m
//  YahooDenaStudy
//
//  Created by Masafumi Yoshida on 2014/07/11.
//  Copyright (c) 2014年 DeNA. All rights reserved.
//

#import <PromiseKit/PromiseKit.h>
#import "UIView+AnyPromise.h"


#define CopyPasta \
    NSAssert([NSThread isMainThread], @"UIKit animation must be performed on the main thread"); \
    \
    if (![NSThread isMainThread]) { \
        id error = [NSError errorWithDomain:PMKErrorDomain code:PMKInvalidUsageError userInfo:@{NSLocalizedDescriptionKey: @"Animation was attempted on a background thread"}]; \
        return [AnyPromise promiseWithValue:error]; \
    } \
    \
    PMKResolver resolve = nil; \
    AnyPromise *promise = [[AnyPromise alloc] initWithResolver:&resolve];


@implementation UIView (PromiseKit)

+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration animations:(void (^)(void))animations {
    return [self promiseWithDuration:duration delay:0 options:0 animations:animations];
}

+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration delay:(NSTimeInterval)delay options:(UIViewAnimationOptions)options animations:(void(^)(void))animations
{
    CopyPasta;

    [UIView animateWithDuration:duration delay:delay options:options animations:animations completion:^(BOOL finished) {
        resolve(@(finished));
    }];

    return promise;
}

+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration delay:(NSTimeInterval)delay usingSpringWithDamping:(CGFloat)dampingRatio initialSpringVelocity:(CGFloat)velocity options:(UIViewAnimationOptions)options animations:(void(^)(void))animations
{
    CopyPasta;

    [UIView animateWithDuration:duration delay:delay usingSpringWithDamping:dampingRatio initialSpringVelocity:velocity options:options animations:animations completion:^(BOOL finished) {
        resolve(@(finished));
    }];

    return promise;
}

+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration delay:(NSTimeInterval)delay options:(UIViewKeyframeAnimationOptions)options keyframeAnimations:(void(^)(void))animations
{
    CopyPasta;

    [UIView animateKeyframesWithDuration:duration delay:delay options:options animations:animations completion:^(BOOL finished) {
        resolve(@(finished));
    }];

    return promise;
}

@end
