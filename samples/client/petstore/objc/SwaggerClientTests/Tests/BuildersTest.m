//
//  BuildersTest.m
//  SwaggerClient
//
//  Created by mmackowiak on 16.05.2016.
//  Copyright (c) 2016 geekerzp. All rights reserved.
//

#import <XCTest/XCTest.h>
#import <SwaggerClient/SWGPet.h>
#import "SWGPetManagedObject.h"
#import "SWGPetManagedObjectBuilder.h"
#import "DatabaseHelper.h"

@interface BuildersTest : XCTestCase {
    SWGPet *pet;
    SWGPetManagedObjectBuilder* builder;
    NSManagedObjectContext *context;
}

@end

@implementation BuildersTest

- (void)setUp {
    [super setUp];

    NSDictionary *petDict = @{ @"id": @1, @"name": @"test pet",
            @"status": @"sold",
            @"photoUrls": @[@"string"],
            @"category": @{ @"id": @1, @"name": @"test category" },
            @"tags": @[ @{ @"id": @1, @"name": @"test tag" }],
            };
    pet = [[SWGPet alloc] initWithDictionary:petDict error:nil];

    context = [DatabaseHelper createContextWithModelName:@"SWGModel"];

    builder = [[SWGPetManagedObjectBuilder alloc] init];
}

- (void)tearDown {
    // Put teardown code here. This method is called after the invocation of each test method in the class.
    [super tearDown];
}

- (void)testExample {
    SWGPetManagedObject * managedObject = [builder SWGPetManagedObjectFromSWGPet:pet context:context];
    SWGPet *pet2 = [builder SWGPetFromSWGPetManagedObject:managedObject];
    [context deleteObject:managedObject];
    SWGTag * tag = [pet.tags firstObject];
    SWGTag * tag2 = [pet2.tags firstObject];
    XCTAssertEqualObjects(tag._id, tag2._id);
    XCTAssertEqualObjects(tag.name, tag2.name);

    SWGCategory * category = pet.category;
    SWGCategory * category2 = pet2.category;

    XCTAssertEqualObjects(category._id, category2._id);
    XCTAssertEqualObjects(category.name, category2.name);

    XCTAssertEqualObjects(pet.status,pet2.status);
    XCTAssertEqualObjects(pet.photoUrls,pet2.photoUrls);
    XCTAssertEqualObjects(pet.name,pet2.name);
    XCTAssertEqualObjects(pet._id,pet2._id);

}

@end
