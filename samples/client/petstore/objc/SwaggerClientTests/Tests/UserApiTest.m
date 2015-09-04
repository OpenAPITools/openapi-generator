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
    
    [self.api loginUserWithCompletionBlock:@"test username" password:@"test password" completionHandler:^(NSString *output, NSError *error) {
        if (error) {
            XCTFail(@"got error %@", error);
        }
        
        if (!output) {
            XCTFail(@"response can't be nil");
        }
        
        NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"logged in user"
                                                                               options:0
                                                                                 error:nil];
        NSTextCheckingResult *match = [regex firstMatchInString:output
                                                        options:0
                                                          range:NSMakeRange(0, [output length])];
        XCTAssertNotNil(match);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}

@end
