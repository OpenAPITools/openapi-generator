#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <SwaggerClient/SWGStoreApi.h>

@interface StoreApiTest : XCTestCase

@property (nonatomic) SWGStoreApi *api;

@end

@implementation StoreApiTest

- (void)setUp {
    [super setUp];
    self.api = [[SWGStoreApi alloc] init];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testGetInventory {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetByStatus"];
    
    [self.api getInventoryWithCompletionHandler:^(NSDictionary *output, NSError *error) {
        
        if (error) {
            XCTFail(@"got error %@", error);
        }
        
        if (!output) {
            XCTFail(@"failed to fetch inventory");
        }
        
        XCTAssertNotNil(output.allKeys);
        
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}


@end
