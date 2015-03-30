#import "PetApiTest.h"
#import "SWGFile.h"

@implementation PetApiTest

- (void)setUp {
    [super setUp];
    api = [[SWGPetApi alloc ]init];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testCreateAndGetPet {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetById"];
    SWGPet* pet = [self createPet];

    [api addPetWithCompletionBlock:pet completionHandler:^(NSError *error) {
        if(error){
            XCTFail(@"got error %@", error);
        }
        NSLog(@"%@", [pet _id]);
        [api getPetByIdWithCompletionBlock:[pet _id] completionHandler:^(SWGPet *output, NSError *error) {
            if(error){
                XCTFail(@"got error %@", error);
            }
            if(output){
                XCTAssertNotNil([output _id], @"token was nil");
            }
            [expectation fulfill];
        }];
    }];
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}

- (void) testUpdatePet {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testUpdatePet"];
    SWGPet* pet = [self createPet];

    [api addPetWithCompletionBlock:pet completionHandler:^(NSError *error) {
        if(error) {
            XCTFail(@"got error %@", error);
        }
        else {
            [api getPetByIdWithCompletionBlock:[NSString stringWithFormat:@"%@",[pet _id]] completionHandler:^(SWGPet *output, NSError *error) {
                if(error) {
                    XCTFail(@"got error %@", error);
                }
                if(output == nil){
                    NSLog(@"failed to fetch pet");
                }
                else {
                    SWGPet* pet = [[SWGPet alloc] initWithDictionary:[output toDictionary] error:nil];
                    NSLog(@"got the pet");

                    [pet setName:@"programmer"];
                    [pet setStatus:@"confused"];

                    [api updatePetWithCompletionBlock:pet
                                    completionHandler:^(NSError *error) {
                                        if(error) {
                                            XCTFail(@"got error %@", error);
                                        }
                                        [api getPetByIdWithCompletionBlock:@1000 completionHandler:^(SWGPet *output, NSError *error) {
                                            if(error) {
                                                XCTFail(@"got error %@", error);
                                            }
                                            if(output == nil){
                                                NSLog(@"failed to fetch pet");
                                            }
                                            else {
                                                SWGPet* pet = [[SWGPet alloc] initWithDictionary:[output toDictionary] error:nil];
                                                XCTAssertNotNil([pet _id], @"pet was nil");
                                                XCTAssertEqualObjects([pet name], @"programmer", @"pet name was not updated");
                                                XCTAssertEqualObjects([pet status], @"confused", @"pet status was not updated");
                                            }
                                            [expectation fulfill];

                                        }];
                                    }];
                }
            }];
        }
    }];
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}

- (void)testGetPetByStatus {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetByStatus"];
    SWGPet* pet = [self createPet];

    [api addPetWithCompletionBlock:pet completionHandler:^(NSError *error) {
        if(error) {
            XCTFail(@"got error %@", error);
        }
        NSArray* status = [[NSArray alloc] initWithObjects:@"available", nil];
        [api findPetsByStatusWithCompletionBlock:status completionHandler:^(NSArray *output, NSError *error) {
            if(error) {
                XCTFail(@"got error %@", error);
            }
            if(output == nil){
                XCTFail(@"failed to fetch pets");
            }
            else {
                bool found = false;
                for(SWGPet* fetched in output) {
                    if([pet _id] == [fetched _id]) {
                        found = true;
                    }
                }
                if(found)
                    [expectation fulfill];
            }
        }];
    }];
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}

- (void)testGetPetByTags {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetByTags"];
    SWGPet* pet = [self createPet];
    SWGTag* tag = [[SWGTag alloc] init];
    tag.name = @"tony";
    NSLog(@"%@", pet._id);
    pet.tags = [[NSArray alloc] initWithObjects:tag, nil];

    [api addPetWithCompletionBlock:pet completionHandler:^(NSError *error) {
        if(error) {
            XCTFail(@"got error %@", error);
        }
        NSArray* tags = [[NSArray alloc] initWithObjects:@"tony", nil];

        [api findPetsByTagsWithCompletionBlock:tags completionHandler:^(NSArray *output, NSError *error) {
            if(error){
                XCTFail(@"got error %@", error);
            }
            if(output){
                bool hasTag = false;
                for(SWGPet * fetched in output) {
                    for(SWGTag * tag in [fetched tags]){
                        if(fetched._id == pet._id && [[tag name] isEqualToString:@"tony"])
                            hasTag = true;
                    }
                }
                if(!hasTag)
                    XCTFail(@"failed to find tag in pet");
                if(hasTag)
                    [expectation fulfill];
            }
        }];
    }];
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}

- (void)testDeletePet {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetById"];

    SWGPet* pet = [self createPet];

    [api addPetWithCompletionBlock:pet completionHandler:^(NSError *error) {
        if(error){
            XCTFail(@"got error %@", error);
        }
        [api deletePetWithCompletionBlock:@"" petId:[NSString stringWithFormat:@"%@", [pet _id]] completionHandler:^(NSError *error) {
            if(error){
                XCTFail(@"got error %@", error);
            }
            [api getPetByIdWithCompletionBlock:[pet _id] completionHandler:^(SWGPet *output, NSError *error) {
                if(error) {
                    // good
                    [expectation fulfill];

                }
                else {
                    XCTFail(@"expected a failure");
                }
            }];
        }];
    }];
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}

- (SWGPet*) createPet {
    SWGPet * pet = [[SWGPet alloc] init];
    pet._id = [[NSNumber alloc] initWithLong:[[NSDate date] timeIntervalSince1970]];
    pet.name = @"monkey";
    SWGCategory * category = [[SWGCategory alloc] init];
    category.name = @"super-happy";

    pet.category = category;
    pet.status = @"available";

    NSArray * photos = [[NSArray alloc] initWithObjects:@"http://foo.bar.com/3", @"http://foo.bar.com/4", nil];
    pet.photoUrls = photos;
    return pet;
}
@end
