/*
 * Copyright (c) 2015 Specta Team. All rights reserved.
 */
#import <Foundation/Foundation.h>

// This protocol is used for whitelisting classes for global beforeEach and afterEach blocks.
// If you want a class to participate in those just add this protocol to a category and it will be
// included.
@protocol SPTGlobalBeforeAfterEach <NSObject>

@optional
+ (void)beforeEach;
+ (void)afterEach;

@end
