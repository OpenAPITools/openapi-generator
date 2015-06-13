#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import "SWGStoreApi.h"

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
    
    [self.api getInventoryWithCompletionBlock:^(NSDictionary *output, NSError *error) {
        
        if (error) {
            XCTFail(@"got error %@", error);
        }
        
        if (!output) {
            XCTFail(@"failed to fetch inventory");
        }
        
        NSSet *expectKeys = [NSSet setWithArray:@[@"confused", @"string", @"pending", @"available", @"sold"]];
        NSSet *keys = [NSSet setWithArray:[output allKeys]];
       
        XCTAssertEqualObjects(expectKeys, keys);
        
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}


@end
