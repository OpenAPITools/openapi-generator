#import "PetApiTest.h"
#import "SWGFile.h"

@implementation PetApiTest

- (void)setUp {
    [super setUp];
    api = [[SWGPetApi alloc ]init];
//    [[SWGApiClient sharedClientFromPool]setLoggingEnabled:true];
    [SWGPetApi setBasePath:@"http://localhost:8002/api"];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testGetPetById {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetById"];
    [api getPetByIdWithCompletionBlock:@1 completionHandler:^(SWGPet *output, NSError *error) {
        if(error){
            XCTFail(@"got error %@", error);
        }
        if(output){
            XCTAssertNotNil([output _id], @"token was nil");
        }
        [expectation fulfill];
    }];
    [self waitForExpectationsWithTimeout:2.0 handler:nil];
}

- (void) testAddPet {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testAddPet"];
    
    SWGPet * petToAdd = [[SWGPet alloc] init];
    [petToAdd set_id:@1000];
    NSMutableArray* tags = [[NSMutableArray alloc] init];
    for(int i = 0; i < 5; i++){
        SWGTag * tag = [[SWGTag alloc] init];
        [tag set_id:[NSNumber numberWithInt:i]];
        [tag setName:[NSString stringWithFormat:@"tag-%d", i]];
        [tags addObject:tag];
    }
    [petToAdd setTags:tags];
    [petToAdd setStatus:@"lost"];
    
    SWGCategory * category = [[SWGCategory alloc] init];
    [category setName:@"sold"];
    [petToAdd setCategory:category];
    [petToAdd setName:@"dragon"];
    
    NSMutableArray* photos = [[NSMutableArray alloc] init];
    for(int i = 0; i < 10; i++){
        NSString * url = [NSString stringWithFormat:@"http://foo.com/photo/%d", i];
        [photos addObject:url];
    }
    [petToAdd setPhotoUrls:photos];
    
    [api addPetWithCompletionBlock:petToAdd completionHandler:^(NSError *error) {
        if(error){
            XCTFail(@"got error %@", error);
        }
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:2.0 handler:nil];
}

- (void) testUpdatePet {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testUpdatePet"];
    SWGPet * petToAdd = [[SWGPet alloc] init];
    [petToAdd set_id:[NSNumber numberWithInt:1000]];
    NSMutableArray* tags = [[NSMutableArray alloc] init];
    for(int i = 0; i < 5; i++){
        SWGTag * tag = [[SWGTag alloc] init];
        [tag set_id:[NSNumber numberWithInt:i]];
        [tag setName:[NSString stringWithFormat:@"tag-%d", i]];
        [tags addObject:tag];
    }
    [petToAdd setTags:tags];
    [petToAdd setStatus:@"lost"];
    
    SWGCategory * category = [[SWGCategory alloc] init];
    [category setName:@"sold"];
    [petToAdd setCategory:category];
    [petToAdd setName:@"dragon"];
    
    NSMutableArray* photos = [[NSMutableArray alloc] init];
    for(int i = 0; i < 10; i++){
        NSString * url = [NSString stringWithFormat:@"http://foo.com/photo/%d", i];
        [photos addObject:url];
    }
    [petToAdd setPhotoUrls:photos];

    [api addPetWithCompletionBlock:petToAdd completionHandler:^(NSError *error) {
        if(error) {
            XCTFail(@"got error %@", error);
        }
        else {
            [api getPetByIdWithCompletionBlock:[NSString stringWithFormat:@"%@",[petToAdd _id]] completionHandler:^(SWGPet *output, NSError *error) {
                if(error) {
                    XCTFail(@"got error %@", error);
                }
                if(output == nil){
                    NSLog(@"failed to fetch pet");
                }
                else {
                    SWGPet* pet = [[SWGPet alloc] initWithValues:[output asDictionary]];
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
                                                SWGPet* pet = [[SWGPet alloc] initWithValues:[output asDictionary]];
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
    [self waitForExpectationsWithTimeout:2.0 handler:nil];
}

- (void)testGetPetByStatus {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetByStatus"];
    [api findPetsByStatusWithCompletionBlock:@"available" completionHandler:^(NSArray *output, NSError *error) {
        if(error) {
            XCTFail(@"got error %@", error);
        }
        if(output == nil){
            XCTFail(@"failed to fetch pets");
        }
        else {
            [expectation fulfill];
        }
    }];
    [self waitForExpectationsWithTimeout:2.0 handler:nil];
}

- (void)testGetPetByTags {
    XCTestExpectation *expectation = [self expectationWithDescription:@"testGetPetByTags"];
    [api findPetsByTagsWithCompletionBlock:@"tag1,tag2" completionHandler:^(NSArray *output, NSError *error) {
        if(error){
            XCTFail(@"got error %@", error);
        }
        if(output){
            for(SWGPet * pet in output) {
                bool hasTag = false;
                for(SWGTag * tag in [pet tags]){
                    if([[tag name] isEqualToString:@"tag1"] || [[tag name] isEqualToString:@"tag2"])
                        hasTag = true;
                }
                if(!hasTag)
                    XCTFail(@"failed to find tag in pet");
            }
        }
        [expectation fulfill];
    }];
    [self waitForExpectationsWithTimeout:2.0 handler:nil];
}
@end