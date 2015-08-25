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

- (void)testSanitizeForDeserialization {
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
    data = [NSDate dateWithISO8601String:@"1997-07-16T19:20:30.45+01:0"];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, [data ISO8601String]);
    
    // model
    data = [self createPet];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, [data toDictionary]);
    
    // NSArray
    data = @[@1];
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
    
    // NSDictionary
    data = @{@"test key": @"test value"};
    result = [self.apiClient sanitizeForSerialization:data];
    XCTAssertEqualObjects(result, data);
}

- (SWGPet*) createPet {
    SWGPet * pet = [[SWGPet alloc] init];
    pet._id = [[NSNumber alloc] initWithLong:[[NSDate date] timeIntervalSince1970]];
    pet.name = @"monkey";
    
    SWGCategory * category = [[SWGCategory alloc] init];
    category._id = [[NSNumber alloc] initWithInteger:arc4random_uniform(100000)];
    category.name = @"super-happy";
    pet.category = category;
    
    SWGTag *tag1 = [[SWGTag alloc] init];
    tag1._id = [[NSNumber alloc] initWithInteger:arc4random_uniform(100000)];
    tag1.name = @"test tag 1";
    SWGTag *tag2 = [[SWGTag alloc] init];
    tag2._id = [[NSNumber alloc] initWithInteger:arc4random_uniform(100000)];
    tag2.name = @"test tag 2";
    pet.tags = (NSArray<SWGTag> *)[[NSArray alloc] initWithObjects:tag1, tag2, nil];

    pet.status = @"available";

    NSArray * photos = [[NSArray alloc] initWithObjects:@"http://foo.bar.com/3", @"http://foo.bar.com/4", nil];
    pet.photoUrls = photos;
    return pet;
}

@end
