#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <ISO8601/ISO8601.h>
#import <SwaggerClient/SWGApiClient.h>
#import <SwaggerClient/SWGDefaultConfiguration.h>
#import <SwaggerClient/SWGQueryParamCollection.h>
#import <SwaggerClient/SWGPet.h>

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
    SWGSanitizer * sanitizer = [[SWGSanitizer alloc] init];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"application/json");
    
    accepts = @[@"application/json", @"application/xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"application/json");
    
    accepts = @[@"APPLICATION/xml", @"APPLICATION/json"];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"application/json");

    accepts = @[@"application/vnd.github+json", @"application/vnd.github+xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"application/json");

    accepts = @[@"application/json;charset=utf-8", @"application/vnd.github+xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"application/json");

    accepts = @[@"application/vnd.github.v3.html+json", @"application/vnd.github+xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"application/json");

    accepts = @[@"application/vnd.github.v3.html+json"];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"application/json");
    
    accepts = @[@"text/plain", @"application/xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"text/plain, application/xml");
    
    accepts = @[];
    XCTAssertEqualObjects([sanitizer selectHeaderAccept:accepts], @"");
}

- (void)testSelectHeaderContentType {
    NSArray *contentTypes = nil;
    SWGSanitizer * sanitizer = [[SWGSanitizer alloc] init];

    contentTypes = @[@"APPLICATION/JSON", @"APPLICATION/XML"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");
    
    contentTypes = @[@"application/json", @"application/xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");
    
    contentTypes = @[@"APPLICATION/xml", @"APPLICATION/json"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");

    contentTypes = @[@"application/vnd.github+json", @"application/vnd.github+xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");

    contentTypes = @[@"application/json;charset=utf-8", @"application/vnd.github+xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");

    contentTypes = @[@"application/json;charset&#x3d;utf-8", @"application/vnd.github+xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");

    contentTypes = @[@"application/vnd.github.v3.html+json", @"application/vnd.github+xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");

    contentTypes = @[@"text/plain", @"application/xml"];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"text/plain");
    
    contentTypes = @[];
    XCTAssertEqualObjects([sanitizer selectHeaderContentType:contentTypes], @"application/json");
}

- (void)testConfiguration {
    SWGDefaultConfiguration *config = [SWGDefaultConfiguration sharedConfig];
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
    SWGDefaultConfiguration *config = [SWGDefaultConfiguration sharedConfig];
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
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSString
    data = @"test string";
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSNumber
    data = @1;
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // SWGQueryParamCollection
    data = [[SWGQueryParamCollection alloc] init];
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSDate
    data = [NSDate dateWithISO8601String:@"1997-07-16T19:20:30.45+01:00"];
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, [data ISO8601StringWithTimeZone:nil usingCalendar:nil]);
    
    data = [NSDate dateWithISO8601String:@"1997-07-16"];
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, [data ISO8601StringWithTimeZone:nil usingCalendar:nil]);
    
    // model
    NSDictionary *petDict = @{@"id": @1, @"name": @"monkey",
                              @"category": @{@"id": @1, @"name": @"test category"},
                              @"tags": @[@{@"id": @1, @"name": @"test tag1"},
                                         @{@"id": @2, @"name": @"test tag2"}],
                              @"status": @"available",
                              @"photoUrls": @[@"http://foo.bar.com/3", @"http://foo.bar.com/4"]};
    data = [[SWGPet alloc] initWithDictionary:petDict error:nil];
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, petDict);
    
    // NSArray
    data = @[@1];
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSArray of models
    NSArray *arrayOfPetDict = @[petDict];
    data = @[[[SWGPet alloc] initWithDictionary:petDict error:nil]];
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, arrayOfPetDict);
    
    // NSDictionary
    data = @{@"test key": @"test value"};
    result = [self.apiClient.sanitizer sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
}

@end
