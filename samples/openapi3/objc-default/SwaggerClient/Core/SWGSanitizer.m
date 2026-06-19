#import "SWGSanitizer.h"
#import "SWGObject.h"
#import "SWGQueryParamCollection.h"
#import "SWGDefaultConfiguration.h"
#import <ISO8601/ISO8601.h>

NSString * const kSWGApplicationJSONType = @"application/json";

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

@property (nonatomic, strong) NSRegularExpression* jsonHeaderTypeExpression;

@end

@implementation SWGSanitizer

-(instancetype)init {
    self = [super init];
    if ( !self ) {
        return nil;
    }
    _jsonHeaderTypeExpression = [NSRegularExpression regularExpressionWithPattern:@"(.*)application(.*)json(.*)" options:NSRegularExpressionCaseInsensitive error:nil];
    return self;
}


- (id) sanitizeForSerialization:(id) object {
    if (object == nil) {
        return nil;
    }
    else if ([object isKindOfClass:[NSString class]] || [object isKindOfClass:[NSNumber class]] || [object isKindOfClass:[SWGQueryParamCollection class]]) {
        return object;
    }
    else if ([object isKindOfClass:[NSDate class]]) {
        return [SWGSanitizer dateToString:object];
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
        return [SWGSanitizer dateToString:param];
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

+ (NSString *)dateToString:(id)date {
    NSTimeZone* timeZone = [SWGDefaultConfiguration sharedConfig].serializationTimeZone;
    return [date ISO8601StringWithTimeZone:timeZone usingCalendar:nil];
}

#pragma mark - Utility Methods

/*
 * Detect `Accept` from accepts
 */
- (NSString *) selectHeaderAccept:(NSArray *)accepts {
    if (accepts.count == 0) {
        return @"";
    }
    NSMutableArray *lowerAccepts = [[NSMutableArray alloc] initWithCapacity:[accepts count]];
    for (NSString *string in accepts) {
        if ([self.jsonHeaderTypeExpression matchesInString:string options:0 range:NSMakeRange(0, [string length])].count > 0) {
            return kSWGApplicationJSONType;
        }
        [lowerAccepts addObject:[string lowercaseString]];
    }
    return [lowerAccepts componentsJoinedByString:@", "];
}

/*
 * Detect `Content-Type` from contentTypes
 */
- (NSString *) selectHeaderContentType:(NSArray *)contentTypes {
    if (contentTypes.count == 0) {
        return kSWGApplicationJSONType;
    }
    NSMutableArray *lowerContentTypes = [[NSMutableArray alloc] initWithCapacity:[contentTypes count]];
    for (NSString *string in contentTypes) {
        if([self.jsonHeaderTypeExpression matchesInString:string options:0 range:NSMakeRange(0, [string length])].count > 0){
            return kSWGApplicationJSONType;
        }
        [lowerContentTypes addObject:[string lowercaseString]];
    }
    return [lowerContentTypes firstObject];
}

@end
