#import "OAIBasicAuthTokenProvider.h"

@implementation OAIBasicAuthTokenProvider

+ (NSString *)createBasicAuthTokenWithUsername:(NSString *)username password:(NSString *)password {

    // return empty string if username and password are empty
    if (username.length == 0 && password.length == 0){
        return  @"";
    }

    NSString *basicAuthCredentials = [NSString stringWithFormat:@"%@:%@", username, password];
    NSData *data = [basicAuthCredentials dataUsingEncoding:NSUTF8StringEncoding];
    basicAuthCredentials = [NSString stringWithFormat:@"Basic %@", [data base64EncodedStringWithOptions:0]];

    return basicAuthCredentials;
}

@end
