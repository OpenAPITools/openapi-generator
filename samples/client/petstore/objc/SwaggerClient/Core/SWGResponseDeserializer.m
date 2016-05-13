#import "SWGResponseDeserializer.h"
#import <JSONModel/JSONModel.h>
#import <ISO8601/ISO8601.h>

NSString *const SWGDeserializationErrorDomainKey = @"SWGDeserializationErrorDomainKey";

NSInteger const SWGTypeMismatchErrorCode = 143553;

NSInteger const SWGEmptyValueOccurredErrorCode = 143509;

NSInteger const SWGUnknownResponseObjectErrorCode = 143528;


@interface SWGResponseDeserializer ()

@property (nonatomic, strong) NSNumberFormatter* numberFormatter;
@property (nonatomic, strong) NSArray *primitiveTypes;
@property (nonatomic, strong) NSArray *basicReturnTypes;

@property (nonatomic, strong) NSRegularExpression* arrayOfModelsPatExpression;
@property (nonatomic, strong) NSRegularExpression* arrayOfPrimitivesPatExpression;
@property (nonatomic, strong) NSRegularExpression* dictPatExpression;
@property (nonatomic, strong) NSRegularExpression* dictModelsPatExpression;

@end

@implementation SWGResponseDeserializer

- (instancetype)init {
    self = [super init];
    if (self) {
        NSNumberFormatter *formatter = [[NSNumberFormatter alloc] init];
        formatter.numberStyle = NSNumberFormatterDecimalStyle;
        _numberFormatter = formatter;
        _primitiveTypes = @[@"NSString", @"NSDate", @"NSNumber"];
        _basicReturnTypes = @[@"NSObject", @"id", @"NSData"];
        _arrayOfModelsPatExpression = [NSRegularExpression regularExpressionWithPattern:@"NSArray<(.+)>"
                                                                                options:NSRegularExpressionCaseInsensitive
                                                                                  error:nil];
        _arrayOfPrimitivesPatExpression = [NSRegularExpression regularExpressionWithPattern:@"NSArray\\* /\\* (.+) \\*/"
                                                                                    options:NSRegularExpressionCaseInsensitive
                                                                                      error:nil];
        _dictPatExpression = [NSRegularExpression regularExpressionWithPattern:@"NSDictionary\\* /\\* (.+?), (.+) \\*/"
                                                                       options:NSRegularExpressionCaseInsensitive
                                                                         error:nil];
        _dictModelsPatExpression = [NSRegularExpression regularExpressionWithPattern:@"NSDictionary\\<(.+?), (.+)*\\>"
                                                                             options:NSRegularExpressionCaseInsensitive
                                                                               error:nil];
    }
    return self;
}

#pragma mark - Deserialize methods

- (id) deserialize:(id) data class:(NSString *) className error:(NSError **) error {
    // return nil if data is nil or className is nil
    if (!data || !className || [data isKindOfClass:[NSNull class]]) {
        return nil;
    }

    // remove "*" from className, if ends with "*"
    if ([className hasSuffix:@"*"]) {
        className = [className substringToIndex:[className length] - 1];
    }
    // pure object
    if ([self.basicReturnTypes containsObject:className]) {
        return data;
    }

    // primitives
    if ([self.primitiveTypes containsObject:className]) {
        return [self deserializePrimitiveValue:data class:className error:error];
    }

    NSTextCheckingResult *match = nil;
    NSRange range = NSMakeRange(0, [className length]);
    // list of models
    match = [self.arrayOfModelsPatExpression firstMatchInString:className options:0 range:range];
    if (match) {
        NSString *innerType = [className substringWithRange:[match rangeAtIndex:1]];
        return [self deserializeArrayValue:data innerType:innerType error:error];
    }

    // list of primitives
    match = [self.arrayOfPrimitivesPatExpression firstMatchInString:className options:0 range:range];
    if (match) {
        NSString *innerType = [className substringWithRange:[match rangeAtIndex:1]];
        return [self deserializeArrayValue:data innerType:innerType error:error];
    }

    // map
    match = [self.dictPatExpression firstMatchInString:className options:0 range:range];
    if (match) {
        NSString *valueType = [className substringWithRange:[match rangeAtIndex:2]];
        return [self deserializeDictionaryValue:data valueType:valueType error:error];
    }

    match = [self.dictModelsPatExpression firstMatchInString:className options:0 range:range];
    if (match) {
        NSString *valueType = [className substringWithRange:[match rangeAtIndex:2]];
        return [self deserializeDictionaryValue:data valueType:valueType error:error];
    }

    // model
    Class ModelClass = NSClassFromString(className);
    if ([ModelClass instancesRespondToSelector:@selector(initWithDictionary:error:)]) {
        return [(JSONModel *) [ModelClass alloc] initWithDictionary:data error:error];
    }

    if(error) {
        *error = [self unknownResponseErrorWithExpectedType:className data:data];
    }
    return nil;
}

- (id) deserializeDictionaryValue:(id) data valueType:(NSString *) valueType error:(NSError**)error {
    if(![data isKindOfClass: [NSDictionary class]]) {
        if(error) {
            *error = [self typeMismatchErrorWithExpectedType:NSStringFromClass([NSDictionary class]) data:data];
        }
        return nil;
    }
    __block NSMutableDictionary *resultDict = [NSMutableDictionary dictionaryWithCapacity:[data count]];
    for (id key in [data allKeys]) {
        id obj = [data valueForKey:key];
        id dicObj = [self deserialize:obj class:valueType error:error];
        if(dicObj) {
            [resultDict setValue:dicObj forKey:key];
        } else if([obj isKindOfClass:[NSNull class]]) {
            if(self.treatNullAsError) {
                if (error) {
                   *error = [self emptyValueOccurredError];
                }
                resultDict = nil;
                break;
            }
        } else {
            resultDict = nil;
            break;
        }
    }
    return resultDict;
}

- (id) deserializeArrayValue:(id) data innerType:(NSString *) innerType error:(NSError**)error {
    if(![data isKindOfClass: [NSArray class]]) {
        if(error) {
            *error = [self typeMismatchErrorWithExpectedType:NSStringFromClass([NSArray class]) data:data];
        }
        return nil;
    }
    NSMutableArray* resultArray = [NSMutableArray arrayWithCapacity:[data count]];
    for (id obj in data) {
        id arrObj = [self deserialize:obj class:innerType error:error];
        if(arrObj) {
            [resultArray addObject:arrObj];
        } else if([obj isKindOfClass:[NSNull class]]) {
            if(self.treatNullAsError) {
                if (error) {
                    *error = [self emptyValueOccurredError];
                }
                resultArray = nil;
                break;
            }
        } else {
            resultArray = nil;
            break;
        }
    }
    return resultArray;
};

- (id) deserializePrimitiveValue:(id) data class:(NSString *) className error:(NSError**)error {
    if ([className isEqualToString:@"NSString"]) {
        return [NSString stringWithString:data];
    }
    else if ([className isEqualToString:@"NSDate"]) {
        return [self deserializeDateValue:data error:error];
    }
    else if ([className isEqualToString:@"NSNumber"]) {
        // NSNumber from NSNumber
        if ([data isKindOfClass:[NSNumber class]]) {
            return data;
        }
        else if ([data isKindOfClass:[NSString class]]) {
            // NSNumber (NSCFBoolean) from NSString
            if ([[data lowercaseString] isEqualToString:@"true"] || [[data lowercaseString] isEqualToString:@"false"]) {
                return @([data boolValue]);
                // NSNumber from NSString
            } else {
                NSNumber* formattedValue = [self.numberFormatter numberFromString:data];
                if(!formattedValue && [data length] > 0 && error) {
                    *error = [self typeMismatchErrorWithExpectedType:className data:data];
                }
                return formattedValue;
            }
        }
    }
    if(error) {
        *error = [self typeMismatchErrorWithExpectedType:className data:data];
    }
    return nil;
}

- (id) deserializeDateValue:(id) data error:(NSError**)error {
    NSDate *date =[NSDate dateWithISO8601String:data];
    if(!date && [data length] > 0 && error) {
        *error = [self typeMismatchErrorWithExpectedType:NSStringFromClass([NSDate class]) data:data];
    }
    return date;
};

-(NSError *)typeMismatchErrorWithExpectedType:(NSString *)expected data:(id)data {
    NSString * message = [NSString stringWithFormat:NSLocalizedString(@"Received response [%@] is not an object of type %@",nil),data, expected];
    NSDictionary * userInfo = @{NSLocalizedDescriptionKey : message};
    return [NSError errorWithDomain:SWGDeserializationErrorDomainKey code:SWGTypeMismatchErrorCode userInfo:userInfo];
}

-(NSError *)emptyValueOccurredError {
    NSString * message = NSLocalizedString(@"Received response contains null value in dictionary or array response",nil);
    NSDictionary * userInfo = @{NSLocalizedDescriptionKey : message};
    return [NSError errorWithDomain:SWGDeserializationErrorDomainKey code:SWGEmptyValueOccurredErrorCode userInfo:userInfo];
}

-(NSError *)unknownResponseErrorWithExpectedType:(NSString *)expected data:(id)data  {
    NSString * message = [NSString stringWithFormat:NSLocalizedString(@"Unknown response expected type %@ [reponse: %@]",nil),expected,data];
    NSDictionary * userInfo = @{NSLocalizedDescriptionKey : message};
    return [NSError errorWithDomain:SWGDeserializationErrorDomainKey code:SWGUnknownResponseObjectErrorCode userInfo:userInfo];
}

@end
