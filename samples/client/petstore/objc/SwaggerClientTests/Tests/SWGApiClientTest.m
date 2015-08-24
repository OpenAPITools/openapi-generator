#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <SwaggerClient/SWGApiClient.h>
#import <SwaggerClient/SWGConfiguration.h>

@interface SWGApiClientTest : XCTestCase

@property (nonatomic) SWGApiClient *apiClient;

@end

@implementation SWGApiClientTest

- (void)setUp {
    [super setUp];
    self.apiClient = [[SWGApiClient alloc] init];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    [super tearDown];
}

- (void)testSelectHeaderAccept {
    NSArray *accepts = nil;
    
    accepts = @[@"APPLICATION/JSON", @"APPLICATION/XML"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderAccept:accepts], @"application/json");
    
    accepts = @[@"application/json", @"application/xml"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderAccept:accepts], @"application/json");
    
    accepts = @[@"APPLICATION/xml", @"APPLICATION/json"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderAccept:accepts], @"application/json");
    
    accepts = @[@"text/plain", @"application/xml"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderAccept:accepts], @"text/plain, application/xml");
    
    accepts = @[];
    XCTAssertEqualObjects([SWGApiClient selectHeaderAccept:accepts], @"");
}

- (void)testSelectHeaderContentType {
    NSArray *contentTypes = nil;
    
    contentTypes = @[@"APPLICATION/JSON", @"APPLICATION/XML"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderContentType:contentTypes], @"application/json");
    
    contentTypes = @[@"application/json", @"application/xml"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderContentType:contentTypes], @"application/json");
    
    contentTypes = @[@"APPLICATION/xml", @"APPLICATION/json"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderContentType:contentTypes], @"application/json");
    
    contentTypes = @[@"text/plain", @"application/xml"];
    XCTAssertEqualObjects([SWGApiClient selectHeaderContentType:contentTypes], @"text/plain");
    
    contentTypes = @[];
    XCTAssertEqualObjects([SWGApiClient selectHeaderContentType:contentTypes], @"application/json");
}

- (void)testConfiguration {
    SWGConfiguration *config = [SWGConfiguration sharedConfig];
    [config setApiKey:@"123456" forApiKeyIdentifier:@"api_key"];
    [config setApiKeyPrefix:@"PREFIX" forApiKeyPrefixIdentifier:@"api_key"];
    config.username = @"test_username";
    config.password = @"test_password";
    
    NSDictionary *headerParams = @{@"test1": @"value1"};
    NSDictionary *queryParams = @{@"test2": @"value2"};
    NSArray *authSettings = @[@"api_key", @"unknown"];
    
    // test prefix
    XCTAssertEqualObjects(@"PREFIX", config.apiKeyPrefix[@"api_key"]);
    [self.apiClient updateHeaderParams:&headerParams
                           queryParams:&queryParams
                      WithAuthSettings:authSettings];
    
    // test api key auth
    XCTAssertEqualObjects(headerParams[@"test1"], @"value1");
    XCTAssertEqualObjects(headerParams[@"api_key"], @"PREFIX 123456");
    XCTAssertEqualObjects(queryParams[@"test2"], @"value2");
    
    // test basic auth
    XCTAssertEqualObjects(@"test_username", config.username);
    XCTAssertEqualObjects(@"test_password", config.password);
}

- (void)testGetBasicAuthToken {
    SWGConfiguration *config = [SWGConfiguration sharedConfig];
    config.username = @"test_username";
    config.password = @"test_password";
    
    NSString *basicAuthCredentials = [NSString stringWithFormat:@"%@:%@", config.username, config.password];
    NSData *data = [basicAuthCredentials dataUsingEncoding:NSUTF8StringEncoding];
    basicAuthCredentials = [NSString stringWithFormat:@"Basic %@", [data base64EncodedStringWithOptions:0]];
    
    XCTAssertEqualObjects(basicAuthCredentials, [config getBasicAuthToken]);
}

@end
