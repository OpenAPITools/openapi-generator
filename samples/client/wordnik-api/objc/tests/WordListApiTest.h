#import <SenTestingKit/SenTestingKit.h>

#import "NIKWordListApi.h"
#import "NIKWordListsApi.h"
#import "NIKAccountApi.h"

@interface WordListApiTest : SenTestCase {
    
@private
    NSString * apiKey;
    NSString * username;
    NSString * password;
    
    NIKWordList * sampleList;
    NIKAuthenticationToken * auth;
    NIKWordListApi * api;
    NIKWordListsApi * listsApi;
}
@end
