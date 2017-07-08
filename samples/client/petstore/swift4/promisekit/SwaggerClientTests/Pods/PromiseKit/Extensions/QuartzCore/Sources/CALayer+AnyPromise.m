//
//  CALayer+PromiseKit.m
//
//  Created by María Patricia Montalvo Dzib on 24/11/14.
//  Copyright (c) 2014 Aluxoft SCP. All rights reserved.
//

#import <QuartzCore/CAAnimation.h>
#import "CALayer+AnyPromise.h"

@interface PMKCAAnimationDelegate : NSObject <CAAnimationDelegate> {
@public
    PMKResolver resolve;
    CAAnimation *animation;
}
@end

@implementation PMKCAAnimationDelegate

- (void)animationDidStop:(CAAnimation *)ignoreOrRetainCycleHappens finished:(BOOL)flag {
    resolve(PMKManifold(@(flag), animation));
    animation.delegate = nil;
}

@end

@implementation CALayer (PromiseKit)

- (AnyPromise *)promiseAnimation:(CAAnimation *)animation forKey:(NSString *)key {
    PMKCAAnimationDelegate *d = animation.delegate = [PMKCAAnimationDelegate new];
    d->animation = animation;
    [self addAnimation:animation forKey:key];
    return [[AnyPromise alloc] initWithResolver:&d->resolve];
}

@end
