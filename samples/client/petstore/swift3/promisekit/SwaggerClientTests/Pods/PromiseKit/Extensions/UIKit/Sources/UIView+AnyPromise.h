#import <PromiseKit/AnyPromise.h>
#import <UIKit/UIView.h>

//  Created by Masafumi Yoshida on 2014/07/11.
//  Copyright (c) 2014年 DeNA. All rights reserved.

/**
 To import the `UIView` category:

    use_frameworks!
    pod "PromiseKit/UIKit"

 Or `UIKit` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"
 
 And then in your sources:

    @import PromiseKit;
*/
@interface UIView (PromiseKit)

/**
 Animate changes to one or more views using the specified duration.

 @param duration The total duration of the animations, measured in 
 seconds. If you specify a negative value or 0, the changes are made
 without animating them.

 @param animations A block object containing the changes to commit to the
 views.

 @return A promise that fulfills with a boolean NSNumber indicating
 whether or not the animations actually finished.
*/
+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration animations:(void (^)(void))animations;

/**
 Animate changes to one or more views using the specified duration, delay,
 options, and completion handler.
 
 @param duration The total duration of the animations, measured in
 seconds. If you specify a negative value or 0, the changes are made
 without animating them.

 @param delay The amount of time (measured in seconds) to wait before
 beginning the animations. Specify a value of 0 to begin the animations
 immediately.
 
 @param options A mask of options indicating how you want to perform the
 animations. For a list of valid constants, see UIViewAnimationOptions.

 @param animations A block object containing the changes to commit to the
 views.

 @return A promise that fulfills with a boolean NSNumber indicating
 whether or not the animations actually finished.
*/
+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration delay:(NSTimeInterval)delay options:(UIViewAnimationOptions)options animations:(void (^)(void))animations NS_REFINED_FOR_SWIFT;

/**
 Performs a view animation using a timing curve corresponding to the
 motion of a physical spring.

 @return A promise that fulfills with a boolean NSNumber indicating
 whether or not the animations actually finished.
*/
+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration delay:(NSTimeInterval)delay usingSpringWithDamping:(CGFloat)dampingRatio initialSpringVelocity:(CGFloat)velocity options:(UIViewAnimationOptions)options animations:(void (^)(void))animations NS_REFINED_FOR_SWIFT;

/**
 Creates an animation block object that can be used to set up
 keyframe-based animations for the current view.

 @return A promise that fulfills with a boolean NSNumber indicating
 whether or not the animations actually finished.
*/
+ (AnyPromise *)promiseWithDuration:(NSTimeInterval)duration delay:(NSTimeInterval)delay options:(UIViewKeyframeAnimationOptions)options keyframeAnimations:(void (^)(void))animations NS_REFINED_FOR_SWIFT;

@end
