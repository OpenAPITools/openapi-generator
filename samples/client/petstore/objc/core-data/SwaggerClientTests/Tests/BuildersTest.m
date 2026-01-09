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
#import "SWGUserManagedObject.h"
#import "SWGUserManagedObjectBuilder.h"
#import "SWGOrderManagedObjectBuilder.h"

@interface BuildersTest : XCTestCase {
    SWGPet *pet;
    SWGUser* user;
    SWGOrder *order;
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
    NSError * error;
    pet = [[SWGPet alloc] initWithDictionary:petDict error:&error];
    XCTAssertNil(error);

    NSDictionary *userDict = @{
            @"id": @1,
            @"email": @"test@test.com",
            @"firstName": @"firstName",
            @"lastName": @"lastName",
            @"password": @"password",
            @"username": @"username",
            @"phone": @"57676767",
    };
    error = nil;
    user = [[SWGUser alloc] initWithDictionary:userDict error:&error];
    XCTAssertNil(error);

    NSDictionary *orderDict = @{
            @"id": @324,
            @"petId": @234,
            @"quantity": @12,
            @"shipDate": @"1997-07-16T19:20:30+00:00",
            @"status": @"status",
            @"complete": @1,
    };
    error = nil;
    order = [[SWGOrder alloc]  initWithDictionary:orderDict error:&error];
    XCTAssertNil(error);

    context = [DatabaseHelper createContextWithModelName:@"SWGModel"];


}

- (void)testSWGPetExample {
    SWGPetManagedObjectBuilder* builder = [[SWGPetManagedObjectBuilder alloc] init];

    SWGPetManagedObject * managedObject = [builder SWGPetManagedObjectFromSWGPet:pet context:context];
    SWGPet *pet2 = [builder SWGPetFromSWGPetManagedObject:managedObject];
    NSError * error;
    XCTAssertTrue([context save:&error]);
    XCTAssertNil(error);
    [context deleteObject:managedObject];
    XCTAssertTrue([context save:&error]);
    XCTAssertNil(error);
    XCTAssertEqualObjects(pet.description,pet2.description);

}


- (void)testSWGUserExample {
    SWGUserManagedObjectBuilder * builder = [[SWGUserManagedObjectBuilder alloc] init];
    SWGUserManagedObject * managedObject = [builder SWGUserManagedObjectFromSWGUser:user context:context];
    SWGUser *user2 = [builder SWGUserFromSWGUserManagedObject:managedObject];
    NSError * error;
    XCTAssertTrue([context save:&error]);
    XCTAssertNil(error);
    [context deleteObject:managedObject];
    XCTAssertTrue([context save:&error]);
    XCTAssertNil(error);
    XCTAssertEqualObjects(user.description,user2.description);

}

- (void)testSWGOrderExample {
    SWGOrderManagedObjectBuilder * builder = [[SWGOrderManagedObjectBuilder alloc] init];
    SWGOrderManagedObject * managedObject = [builder SWGOrderManagedObjectFromSWGOrder:order context:context];
    SWGOrder *order2 = [builder SWGOrderFromSWGOrderManagedObject:managedObject];
    NSError * error;
    XCTAssertTrue([context save:&error]);
    XCTAssertNil(error);
    [context deleteObject:managedObject];
    XCTAssertTrue([context save:&error]);
    XCTAssertNil(error);

    XCTAssertEqualObjects(order.description,order2.description);

}

@end
