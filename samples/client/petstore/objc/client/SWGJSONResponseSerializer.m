#import "SWGJSONResponseSerializer.h"

static BOOL JSONParseError(NSError *error) {
    if ([error.domain isEqualToString:NSCocoaErrorDomain] && error.code == 3840) {
        return YES;
    }
    
    return NO;
}

@implementation SWGJSONResponseSerializer

- (id) responseObjectForResponse:(NSURLResponse *)response
                            data:(NSData *)data
                           error:(NSError *__autoreleasing *)error {
    NSDictionary *responseJson = [super responseObjectForResponse:response data:data error:error];

    // if response data is not a valid json, return string of data.
    if (JSONParseError(*error)) {
        *error = nil;
        NSString *responseString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        return responseString;
    }
    
    return responseJson;
}

@end
