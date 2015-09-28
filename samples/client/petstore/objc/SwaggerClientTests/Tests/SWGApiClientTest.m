#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <ISO8601/ISO8601.h>
#import <SwaggerClient/SWGApiClient.h>
#import <SwaggerClient/SWGConfiguration.h>
#import <SwaggerClient/SWGQueryParamCollection.h>
#import <SwaggerClient/SWGPet.h>
#import <SwaggerClient/SWGTag.h>
#import <SwaggerClient/SWGCategory.h>

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

- (void)testSanitizeForSerialization {
    id result;
    id data;
    
    // nil
    data = nil;
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSString
    data = @"test string";
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSNumber
    data = @1;
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // SWGQueryParamCollection
    data = [[SWGQueryParamCollection alloc] init];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSDate
    data = [NSDate dateWithISO8601String:@"1997-07-16T19:20:30.45+01:00"];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, [data ISO8601String]);
    
    data = [NSDate dateWithISO8601String:@"1997-07-16"];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, [data ISO8601String]);
    
    // model
    NSDictionary *petDict = @{@"id": @1, @"name": @"monkey",
                              @"category": @{@"id": @1, @"name": @"test category"},
                              @"tags": @[@{@"id": @1, @"name": @"test tag1"},
                                         @{@"id": @2, @"name": @"test tag2"}],
                              @"status": @"available",
                              @"photoUrls": @[@"http://foo.bar.com/3", @"http://foo.bar.com/4"]};
    data = [[SWGPet alloc] initWithDictionary:petDict error:nil];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, petDict);
    
    // NSArray
    data = @[@1];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSArray of models
    NSArray *arrayOfPetDict = @[petDict];
    data = [NSArray arrayWithObject:[[SWGPet alloc] initWithDictionary:petDict error:nil]];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, arrayOfPetDict);
    
    // NSDictionary
    data = @{@"test key": @"test value"};
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
}

@end
