//
//  ISO8601Serialization.h
//  ISO8601
//
//  Created by Sam Soffes on 7/30/14.
//  Copyright (c) 2014 Sam Soffes. All rights reserved.
//

#if __has_feature(modules)
	@import Foundation;
#else
	#import <Foundation/Foundation.h>
#endif

@interface ISO8601Serialization : NSObject

#pragma mark - Reading

/**
 Create date components from an ISO8601 string.
 
 @param string An ISO8601 string.
 
 @return An object containing the date components for a given ISO8601 string.
 */
+ (NSDateComponents *)dateComponentsForString:(NSString *)string;


#pragma mark - Writing

/**
 Create an ISO8601 string from date components.
 
 @param components Date components to use.
 
 @return A string containing the date components as an ISO8601 string.
 */
+ (NSString *)stringForDateComponents:(NSDateComponents *)components;

@end
