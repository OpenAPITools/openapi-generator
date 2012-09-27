#import <SenTestingKit/SenTestingKit.h>
#import "NIKWordsApi.h"

@interface WordsApiTest : SenTestCase {
    
@private
    NIKWordsApi * api;
    NSString * apiKey;
    NSString * username;
    NSString * password;
}

@end
