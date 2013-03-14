#import "PetApiTest.h"
#import "NIKFile.h"

@implementation PetApiTest

- (void)setUp {
    [super setUp];
    api = [[NIKPetApi alloc ]init];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testGetPetById {
    bool done = false;
    static NIKPet* pet = nil;
    static NSError * gError = nil;

    [api getPetByIdWithCompletionBlock:@"1" completionHandler:^(NIKPet *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to fetch pet");
        }
        else {
            pet = [[NIKPet alloc] initWithValues:[output asDictionary]];
        }
    }];

    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(pet){
            STAssertNotNil([pet _id], @"token was nil");
            done = true;
        }
    }
    STAssertNotNil(pet, @"failed to fetch valid result in 10 seconds");
}

- (void) testAddPet {
    bool done = false;
    static NSError * gError = nil;

    NIKPet * petToAdd = [[NIKPet alloc] init];
    [petToAdd set_id:@"1000"];
    NSMutableArray* tags = [[NSMutableArray alloc] init];
    for(int i = 0; i < 5; i++){
        NIKTag * tag = [[NIKTag alloc] init];
        [tag set_id:[NSNumber numberWithInt:i]];
        [tag setName:[NSString stringWithFormat:@"tag-%d", i]];
        [tags addObject:tag];
    }
    [petToAdd setTags:tags];
    [petToAdd setStatus:@"lost"];

    NIKCategory * category = [[NIKCategory alloc] init];
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
            gError = error;
        }
        hasResponse = true;
    }];
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(hasResponse){
            done = true;
        }
    }

    static NIKPet* pet = nil;

    [api getPetByIdWithCompletionBlock:[NSString stringWithFormat:@"%@",[petToAdd _id]] completionHandler:^(NIKPet *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to fetch pet");
        }
        else {
            pet = [[NIKPet alloc] initWithValues:[output asDictionary]];
        }
    }];

    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    done = false;
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(pet){
            STAssertNotNil([pet _id], @"pet was nil");
            done = true;
        }
    }
    STAssertNotNil(pet, @"failed to fetch valid result in 10 seconds");

}

- (void) testUpdatePet {
    bool done = false;
    static NSError * gError = nil;

    NIKPet * petToAdd = [[NIKPet alloc] init];
    [petToAdd set_id:[NSNumber numberWithInt:1000]];
    NSMutableArray* tags = [[NSMutableArray alloc] init];
    for(int i = 0; i < 5; i++){
        NIKTag * tag = [[NIKTag alloc] init];
        [tag set_id:[NSNumber numberWithInt:i]];
        [tag setName:[NSString stringWithFormat:@"tag-%d", i]];
        [tags addObject:tag];
    }
    [petToAdd setTags:tags];
    [petToAdd setStatus:@"lost"];
    
    NIKCategory * category = [[NIKCategory alloc] init];
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
            gError = error;
        }
        hasResponse = true;
    }];
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    done = false;
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            done = true;
            STFail(@"got error %@", gError);
        }
        if(hasResponse){
            done = true;
            NSLog(@"pet was added");
        }
    }
    
    static NIKPet* pet = nil;
    done = false;
    
    [api getPetByIdWithCompletionBlock:[NSString stringWithFormat:@"%@",[petToAdd _id]] completionHandler:^(NIKPet *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to fetch pet");
        }
        else {
            pet = [[NIKPet alloc] initWithValues:[output asDictionary]];
            NSLog(@"got the pet");
        }
    }];
    
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(pet){
            STAssertNotNil([pet _id], @"pet was nil");
            done = true;
        }
    }
    STAssertNotNil(pet, @"failed to fetch valid result in 10 seconds");
    
    [pet setName:@"programmer"];
    [pet setStatus:@"confused"];
    
    hasResponse = false;
    [api updatePetWithCompletionBlock:pet
                    completionHandler:^(NSError *error) {
                        if(error) {
                            gError = error;
                        }
                        hasResponse = true;
                    }];

    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    done = false;
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        else if(hasResponse) {
            done = true;
        }
    }
    
    pet = nil;
    done = false;
    
    [api getPetByIdWithCompletionBlock:[NSString stringWithFormat:@"%d",1000] completionHandler:^(NIKPet *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to fetch pet");
        }
        else {
            pet = [[NIKPet alloc] initWithValues:[output asDictionary]];
        }
    }];
    
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(pet){
            STAssertNotNil([pet _id], @"pet was nil");
            STAssertEqualObjects([pet name], @"programmer", @"pet name was not updated");
            STAssertEqualObjects([pet status], @"confused", @"pet status was not updated");
            done = true;
        }
    }
    STAssertNotNil(pet, @"failed to fetch valid result in 10 seconds");
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
            for(NIKPet* pet in output) {
                [pets addObject:[[NIKPet alloc] initWithValues:[pet asDictionary]]];
            }
        }
    }];
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(pets){
            for(NIKPet * pet in pets) {
                STAssertEqualObjects([pet status], @"available", @"got invalid status for pets");
            }
            done = true;
        }
    }
}

- (void)testGetPetByTags {
    bool done = false;
    static NSMutableArray* pets = nil;
    static NSError * gError = nil;
    [api findPetsByTagsWithCompletionBlock:@"tag1,tag2" completionHandler:^(NSArray *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to fetch pets");
        }
        else {
            pets = [[NSMutableArray alloc]init];
            for(NIKPet* pet in output) {
                [pets addObject:[[NIKPet alloc] initWithValues:[pet asDictionary]]];
            }
        }
    }];
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(pets){
            for(NIKPet * pet in pets) {
                bool hasTag = false;
                for(NIKTag * tag in [pet tags]){
                    if([[tag name] isEqualToString:@"tag1"] || [[tag name] isEqualToString:@"tag2"])
                        hasTag = true;
                }
                if(!hasTag) STFail(@"failed to find tag in pet");
            }
            done = true;
        }
    }
}
@end
