#import "PetApiTest.h"
#import "SWGFile.h"

@implementation PetApiTest

- (void)setUp {
    [super setUp];
    api = [[SWGPetApi alloc ]init];
    [SWGPetApi setBasePath:@"http://localhost:8002/api"];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testGetPetById {
    [api getPetByIdWithCompletionBlock:@1 completionHandler:^(SWGPet *output, NSError *error) {
        if(error){
            XCTFail(@"got error %@", error);
        }
        if(output){
            XCTAssertNotNil([output _id], @"token was nil");
        }
    }];
}

- (void) testAddPet {
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
    }];
}

- (void) testUpdatePet {
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
    
    static bool hasResponse = false;
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
                                        }];
                                    }];
                }
            }];
        }
    }];
}

- (void)testGetPetByStatus {
    bool done = false;
    static NSMutableArray* pets = nil;
    static NSError * gError = nil;
    [api findPetsByStatusWithCompletionBlock:@"available" completionHandler:^(NSArray *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to fetch pets");
        }
        else {
            pets = [[NSMutableArray alloc]init];
            for(SWGPet* pet in output) {
                [pets addObject:[[SWGPet alloc] initWithValues:[pet asDictionary]]];
            }
        }
    }];
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            XCTFail(@"got error %@", gError);
            done = true;
        }
        if(pets){
            for(SWGPet * pet in pets) {
                XCTAssertEqualObjects([pet status], @"available", @"got invalid status for pets");
            }
            done = true;
        }
    }
}

- (void)testGetPetByTags {
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
    }];
}
@end