//
//  EXPMatcher.h
//  Expecta
//
//  Created by Luke Redpath on 26/03/2012.
//  Copyright (c) 2012 Peter Jihoon Kim. All rights reserved.
//

#import <Foundation/Foundation.h>

@protocol EXPMatcher <NSObject>

- (BOOL)matches:(id)actual;

@optional
- (BOOL)meetsPrerequesiteFor:(id)actual;
- (NSString *)failureMessageForTo:(id)actual;
- (NSString *)failureMessageForNotTo:(id)actual;

@end
