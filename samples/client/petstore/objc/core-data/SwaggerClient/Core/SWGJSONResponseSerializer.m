#import "SWGJSONResponseSerializer.h"

@implementation SWGJSONResponseSerializer

///
/// When customize a response serializer,
/// the serializer must conform the protocol `AFURLResponseSerialization`
/// and implements the protocol method `responseObjectForResponse:error:`
///
/// @param response The response to be processed.
/// @param data     The response data to be decoded.
/// @param error    The error that occurred while attempting to decode the response data.
///
/// @return The object decoded from the specified response data.
///
- (id) responseObjectForResponse:(NSURLResponse *)response
                            data:(NSData *)data
                           error:(NSError *__autoreleasing *)error {
    NSDictionary *responseJson = [super responseObjectForResponse:response data:data error:error];

    // if response data is not a valid json, return string of data.
    if ([self isParseError:*error]) {
        *error = nil;
        NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        return responseString;
    }
    
    return responseJson;
}

-(BOOL)isParseError:(NSError *)error {
    return [error.domain isEqualToString:NSCocoaErrorDomain] && error.code == 3840;
}

+ (instancetype)serializer {
    return [self serializerWithReadingOptions:NSJSONReadingAllowFragments];
}

@end
