#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import "SWGApiClient.h"

@interface SWGApiClientTest : XCTestCase

@end

@implementation SWGApiClientTest

- (void)setUp {
    [super setUp];
    // Put setup code here. This method is called before the invocation of each test method in the class.
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

@end
