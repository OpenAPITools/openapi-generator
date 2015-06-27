//
//  ISO8601.h
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

//! Project version number for ISO8601.
FOUNDATION_EXPORT double ISO8601VersionNumber;

//! Project version string for ISO8601.
FOUNDATION_EXPORT const unsigned char ISO8601VersionString[];

#import <ISO8601/ISO8601Serialization.h>
#import <ISO8601/NSDate+ISO8601.h>
