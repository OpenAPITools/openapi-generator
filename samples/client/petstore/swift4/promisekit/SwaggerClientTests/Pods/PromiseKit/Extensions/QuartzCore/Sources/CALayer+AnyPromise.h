//
//  CALayer+AnyPromise.h
//
//  Created by María Patricia Montalvo Dzib on 24/11/14.
//  Copyright (c) 2014 Aluxoft SCP. All rights reserved.
//

#import <QuartzCore/QuartzCore.h>
#import <PromiseKit/AnyPromise.h>

/**
 To import the `CALayer` category:

    use_frameworks!
    pod "PromiseKit/QuartzCore"

 Or `CALayer` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    @import PromiseKit;
*/
@interface CALayer (PromiseKit)

/**
 Add the specified animation object to the layer’s render tree.

 @return A promise that thens two parameters:

  1. `@YES` if the animation progressed entirely to completion.
  2. The `CAAnimation` object.

 @see addAnimation:forKey
*/
- (AnyPromise *)promiseAnimation:(CAAnimation *)animation forKey:(NSString *)key;

@end
