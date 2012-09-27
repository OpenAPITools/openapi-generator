#import "UserApiTest.h"

@implementation UserApiTest

- (void)setUp {
    [super setUp];
    api = [[NIKUserApi alloc ]init];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testLoginUser {
    bool done = false;
    static NSString* authString = nil;
    static NSError * gError = nil;
    
    [api loginUserWithCompletionBlock:@"foo"
                             password:@"bar"
                    completionHandler:^(NSString *output, NSError *error) {
        if(error) {
            gError = error;
        }
        if(output == nil){
            NSLog(@"failed to login");
        }
        else {
            authString = [NSString stringWithString:output];
        }
    }];

    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(authString){
            done = true;
        }
    }
    STAssertTrue([authString length] > 0, @"string was zero chars");
    NSLog(@"got login %@", authString);
}

@end
