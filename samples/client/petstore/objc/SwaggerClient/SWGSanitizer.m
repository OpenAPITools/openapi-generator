#import "SWGSanitizer.h"
#import "SWGObject.h"
#import "SWGQueryParamCollection.h"
#import <ISO8601/ISO8601.h>

NSString * SWGPercentEscapedStringFromString(NSString *string) {
    static NSString * const kSWGCharactersGeneralDelimitersToEncode = @":#[]@";
    static NSString * const kSWGCharactersSubDelimitersToEncode = @"!$&'()*+,;=";

    NSMutableCharacterSet * allowedCharacterSet = [[NSCharacterSet URLQueryAllowedCharacterSet] mutableCopy];
    [allowedCharacterSet removeCharactersInString:[kSWGCharactersGeneralDelimitersToEncode stringByAppendingString:kSWGCharactersSubDelimitersToEncode]];

    static NSUInteger const batchSize = 50;

    NSUInteger index = 0;
    NSMutableString *escaped = @"".mutableCopy;

    while (index < string.length) {
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wgnu"
    NSUInteger length = MIN(string.length - index, batchSize);
    #pragma GCC diagnostic pop
    NSRange range = NSMakeRange(index, length);

    // To avoid breaking up character sequences such as 👴🏻👮🏽
    range = [string rangeOfComposedCharacterSequencesForRange:range];

    NSString *substring = [string substringWithRange:range];
    NSString *encoded = [substring stringByAddingPercentEncodingWithAllowedCharacters:allowedCharacterSet];
    [escaped appendString:encoded];

    index += range.length;
    }

    return escaped;
}

@interface SWGSanitizer ()

@end

@implementation SWGSanitizer

- (id) sanitizeForSerialization:(id) object {
    if (object == nil) {
        return nil;
    }
    else if ([object isKindOfClass:[NSString class]] || [object isKindOfClass:[NSNumber class]] || [object isKindOfClass:[SWGQueryParamCollection class]]) {
        return object;
    }
    else if ([object isKindOfClass:[NSDate class]]) {
        return [object ISO8601String];
    }
    else if ([object isKindOfClass:[NSArray class]]) {
        NSArray *objectArray = object;
        NSMutableArray *sanitizedObjs = [NSMutableArray arrayWithCapacity:[objectArray count]];
        [object enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
            id sanitizedObj = [self sanitizeForSerialization:obj];
            if (sanitizedObj) {
                [sanitizedObjs addObject:sanitizedObj];
            }
        }];
        return sanitizedObjs;
    }
    else if ([object isKindOfClass:[NSDictionary class]]) {
        NSDictionary *objectDict = object;
        NSMutableDictionary *sanitizedObjs = [NSMutableDictionary dictionaryWithCapacity:[objectDict count]];
        [object enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
            id sanitizedObj = [self sanitizeForSerialization:obj];
            if (sanitizedObj) {
                sanitizedObjs[key] = sanitizedObj;
            }
        }];
        return sanitizedObjs;
    }
    else if ([object isKindOfClass:[SWGObject class]]) {
        return [object toDictionary];
    }
    else {
        NSException *e = [NSException
                exceptionWithName:@"InvalidObjectArgumentException"
                           reason:[NSString stringWithFormat:@"*** The argument object: %@ is invalid", object]
                         userInfo:nil];
        @throw e;
    }
}

- (NSString *) parameterToString:(id)param {
    if ([param isKindOfClass:[NSString class]]) {
        return param;
    }
    else if ([param isKindOfClass:[NSNumber class]]) {
        return [param stringValue];
    }
    else if ([param isKindOfClass:[NSDate class]]) {
        return [param ISO8601String];
    }
    else if ([param isKindOfClass:[NSArray class]]) {
        NSMutableArray *mutableParam = [NSMutableArray array];
        [param enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
            [mutableParam addObject:[self parameterToString:obj]];
        }];
        return [mutableParam componentsJoinedByString:@","];
    }
    else {
        NSException *e = [NSException
                exceptionWithName:@"InvalidObjectArgumentException"
                           reason:[NSString stringWithFormat:@"*** The argument object: %@ is invalid", param]
                         userInfo:nil];
        @throw e;
    }
}

#pragma mark - Utility Methods

/*
 * Detect `Accept` from accepts
 */
- (NSString *) selectHeaderAccept:(NSArray *)accepts {
    if (accepts == nil || [accepts count] == 0) {
        return @"";
    }

    NSMutableArray *lowerAccepts = [[NSMutableArray alloc] initWithCapacity:[accepts count]];
    for (NSString *string in accepts) {
        NSString * lowerAccept = [string lowercaseString];
        // use rangeOfString instead of containsString for iOS 7 support
        if ([lowerAccept rangeOfString:@"application/json"].location != NSNotFound) {
            return @"application/json";
        }
        [lowerAccepts addObject:lowerAccept];
    }

    if (lowerAccepts.count == 1) {
        return [lowerAccepts firstObject];
    }

    return [lowerAccepts componentsJoinedByString:@", "];
}

/*
 * Detect `Content-Type` from contentTypes
 */
- (NSString *) selectHeaderContentType:(NSArray *)contentTypes {
    if (contentTypes == nil || [contentTypes count] == 0) {
        return @"application/json";
    }

    NSMutableArray *lowerContentTypes = [[NSMutableArray alloc] initWithCapacity:[contentTypes count]];
    [contentTypes enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
        [lowerContentTypes addObject:[obj lowercaseString]];
    }];

    if ([lowerContentTypes containsObject:@"application/json"]) {
        return @"application/json";
    }
    else {
        return lowerContentTypes[0];
    }
}

@end
