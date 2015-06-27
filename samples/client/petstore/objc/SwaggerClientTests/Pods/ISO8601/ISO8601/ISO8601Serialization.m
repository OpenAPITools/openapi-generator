//
//  ISO8601Serialization.m
//  ISO8601
//
//  Created by Sam Soffes on 7/30/14.
//  Copyright (c) 2014 Sam Soffes. All rights reserved.
//

#import "ISO8601Serialization.h"

@implementation ISO8601Serialization

+ (NSDateComponents *)dateComponentsForString:(NSString *)string {
	NSScanner *scanner = [[NSScanner alloc] initWithString:string];
	scanner.charactersToBeSkipped = nil;
	
	NSDateComponents *dateComponents = [[NSDateComponents alloc] init];
	
	// Year
	NSInteger year;
	if (![scanner scanInteger:&year]) {
		return nil;
	}
	dateComponents.year = year;
	
	// Month
	if (![scanner scanString:@"-" intoString:nil]) {
		return dateComponents;
	}
	
	NSInteger month;
	if (![scanner scanInteger:&month]) {
		return dateComponents;
	}
	dateComponents.month = month;
	
	// Day
	if (![scanner scanString:@"-" intoString:nil]) {
		return dateComponents;
	}
	
	NSInteger day;
	if (![scanner scanInteger:&day]) {
		return dateComponents;
	}
	dateComponents.day = day;
	
	// Time
	if (![scanner scanCharactersFromSet:[NSCharacterSet characterSetWithCharactersInString:@"T "] intoString:nil]) {
		return dateComponents;
	}
	
	// Hour
	NSInteger hour;
	if (![scanner scanInteger:&hour]) {
		return dateComponents;
	}
	dateComponents.hour = hour;
	
	// Minute
	if (![scanner scanString:@":" intoString:nil]) {
		return dateComponents;
	}
	
	NSInteger minute;
	if (![scanner scanInteger:&minute]) {
		return dateComponents;
	}
	dateComponents.minute = minute;

	// Second
	NSUInteger scannerLocation = scanner.scanLocation;
	if ([scanner scanString:@":" intoString:nil]) {
		NSInteger second;
		if (![scanner scanInteger:&second]) {
			return dateComponents;
		}
		dateComponents.second = second;
	} else {
		scanner.scanLocation = scannerLocation;
	}

	// Zulu
	scannerLocation = scanner.scanLocation;
	[scanner scanUpToString:@"Z" intoString:nil];
	if ([scanner scanString:@"Z" intoString:nil]) {
		// Z stands for the Zulu (Z in the NATO phonetic alphabet) time zone. UTC and the Zulu time
		// zone are synonymous.
		dateComponents.timeZone = [NSTimeZone timeZoneForSecondsFromGMT:0];
		return dateComponents;
	}
	
	// Move back to end of time
	scanner.scanLocation = scannerLocation;
	
	// Look for offset
	NSCharacterSet *signs = [NSCharacterSet characterSetWithCharactersInString:@"+-"];
	[scanner scanUpToCharactersFromSet:signs intoString:nil];
	NSString *sign;
	if (![scanner scanCharactersFromSet:signs intoString:&sign]) {
		return dateComponents;
	}
	
	// Offset hour
	NSInteger timeZoneOffset = 0;
	NSInteger timeZoneOffsetHour = 0;
	NSInteger timeZoneOffsetMinute = 0;
	if (![scanner scanInteger:&timeZoneOffsetHour]) {
		return dateComponents;
	}
	
	// Check for colon
	BOOL colonExists = [scanner scanString:@":" intoString:nil];
	if (!colonExists && timeZoneOffsetHour > 14) {
		timeZoneOffsetMinute = timeZoneOffsetHour % 100;
		timeZoneOffsetHour = floor(timeZoneOffsetHour / 100);
	} else {
		// Offset minute
		[scanner scanInteger:&timeZoneOffsetMinute];
	}

	timeZoneOffset = (timeZoneOffsetHour * 60 * 60) + (timeZoneOffsetMinute * 60);
	dateComponents.timeZone = [NSTimeZone timeZoneForSecondsFromGMT:timeZoneOffset * ([sign isEqualToString:@"-"] ? -1 : 1)];

	return dateComponents;
}


+ (NSString *)stringForDateComponents:(NSDateComponents *)components {	
	NSString *string = [[NSString alloc] initWithFormat:@"%04li-%02i-%02iT%02i:%02i:%02i", (long)components.year,
						(int)components.month, (int)components.day, (int)components.hour, (int)components.minute,
						(int)components.second];

	NSTimeZone *timeZone = components.timeZone;
	if (!timeZone) {
		return string;
	}
	
	if (timeZone.secondsFromGMT != 0) {
		NSInteger hoursOffset = timeZone.secondsFromGMT / 3600;
		
		// TODO: Assuming whole hour offsets at the moment
		NSUInteger secondsOffset = 0;
		
		NSString *sign = (hoursOffset >= 0) ? @"+" : @"-";
		return [string stringByAppendingFormat:@"%@%02i:%02i", sign, abs((int)hoursOffset), (int)secondsOffset];
	}
	
	return [string stringByAppendingString:@"Z"];
}

@end
