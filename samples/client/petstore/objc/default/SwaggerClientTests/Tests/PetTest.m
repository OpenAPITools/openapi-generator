#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <SwaggerClient/SWGPet.h>

@interface PetTest : XCTestCase {

@private SWGPet *pet;
    
}

@end

@implementation PetTest

- (void)setUp {
    [super setUp];
    
    NSDictionary *petDict = @{ @"id": @1, @"name": @"test pet",
                               @"status": @"sold",
                               @"photoUrls": @[@"string"],
                               @"category": @{ @"id": @1, @"name": @"test category" },
                               @"tags": @[ @{ @"id": @1, @"name": @"test tag" } ]};
    pet = [[SWGPet alloc] initWithDictionary:petDict error:nil];
}

- (void)testDescription {
    NSDictionary *petDict = @{ @"id": @1, @"name": @"test pet",
                               @"status": @"sold",
                               @"photoUrls": @[@"string"],
                               @"category": @{ @"id": @1, @"name": @"test category" },
                               @"tags": @[ @{ @"id": @1, @"name": @"test tag" } ]};
    NSString *petStr = [petDict description];
    
    XCTAssertTrue([[pet description] isEqualToString:petStr]);
}

@end
