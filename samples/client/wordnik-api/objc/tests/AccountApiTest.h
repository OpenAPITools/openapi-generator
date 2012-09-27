#import <SenTestingKit/SenTestingKit.h>
#import "NIKAccountApi.h"

@interface AccountApiTest : SenTestCase {
    
@private
    NIKAccountApi * api;
    NSString * apiKey;
    NSString * username;
    NSString * password;
}

@end
