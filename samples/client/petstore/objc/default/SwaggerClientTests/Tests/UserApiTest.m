#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <SwaggerClient/SWGUserApi.h>

@interface UserApiTest : XCTestCase

@property (nonatomic) SWGUserApi *api;

@end

@implementation UserApiTest

- (void)setUp {
    [super setUp];
    self.api = [[SWGUserApi alloc] init];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testLoginUser {
    XCTestExpectation *expectation = [self expectationWithDescription:@"test login user"];
    
    [self.api loginUserWithUsername:@"test username" password:@"test password" completionHandler:^(NSString *output, NSError *error) {
        if (error) {
            XCTFail(@"got error %@", error);
        }
        
        if (!output) {
            XCTFail(@"response can't be nil");
        }
        XCTAssertTrue([output rangeOfString:@"logged in user"].location != NSNotFound);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}

@end
