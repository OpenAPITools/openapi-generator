#import "AccountApiTest.h"


@implementation AccountApiTest

- (void)setUp {
    [super setUp];
    api = [[NIKAccountApi alloc ]init];
    apiKey = [[[NSProcessInfo processInfo] environment] objectForKey:@"API_KEY"];
    
    if(apiKey == nil){
        STFail(@"API_KEY environment variable was not set");
    }
    [api addHeader:apiKey forKey:@"api_key"];
    
    username = [[[NSProcessInfo processInfo] environment] objectForKey:@"USER_NAME"];
    if(username == nil){
        STFail(@"USER_NAME environment variable was not set");
    }
    
    password = [[[NSProcessInfo processInfo] environment] objectForKey:@"PASSWORD"];
    if(username == nil){
        STFail(@"PASSWORD environment variable was not set");
    }
}

- (void)tearDown {
    [super tearDown];
}

- (void)testAuthenticate {
    bool done = false;
    static NIKAuthenticationToken* target = nil;
    static NSError * gError = nil;
    
    
    [api authenticateWithCompletionBlock:username
                                password:password
                       completionHandler:^(NIKAuthenticationToken *token, NSError *error) {
                           if(error) {
                               gError = error;
                           }
                           if(token == nil){
                               NSLog(@"failed to get data");
                           }
                           else {
                               target = [[NIKAuthenticationToken alloc] initWithValues:[token asDictionary]];
                           }
                       }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertNotNil([target token], @"token was nil");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testAuthenticatePost {
    bool done = false;
    static NIKAuthenticationToken* target = nil;
    static NSError * gError = nil;
    
    
    [api authenticatePostWithCompletionBlock:username
                                        body:password
                           completionHandler:^(NIKAuthenticationToken * token, NSError * error) {
                               if(error) {
                                   gError = error;
                               }
                               if(token == nil){
                                   NSLog(@"failed to get data");
                               }
                               else {
                                   target = [[NIKAuthenticationToken alloc] initWithValues:[token asDictionary]];
                               }
                           }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertNotNil([target token], @"token was nil");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testGetWordListsForLoggedInUser {
    bool done = false;
    static NIKAuthenticationToken * auth = nil;
    static NSArray* target = nil;
    static NSError * gError = nil;
    
    [api authenticatePostWithCompletionBlock:username
                                        body:password
                           completionHandler:^(NIKAuthenticationToken * authToken, NSError * error) {
                               if(error) {
                                   gError = error;
                               }
                               if(authToken == nil){
                                   NSLog(@"failed to get data");
                               }
                               else {
                                   auth = [[NIKAuthenticationToken alloc] initWithValues:[authToken asDictionary]];
                               }
                           }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(auth){
            STAssertNotNil([auth token], @"token was nil");
            done = true;
        }
    }
    
    done = false;
    [api getWordListsForLoggedInUserWithCompletionBlock:[auth token]
                                                   skip:[NSNumber numberWithInt:0]
                                                  limit:[NSNumber numberWithInt:15]
                                      completionHandler:^(NSArray *lists, NSError *error) {
                                          if(error) {
                                              gError = error;
                                          }
                                          if(lists == nil){
                                              NSLog(@"failed to get data");
                                          }
                                          else {
                                              target = [NSArray arrayWithArray:lists];
                                          }
                                      }];
    
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            for(NIKWordList* list in target){
                STAssertNotNil([list description], [list name], [list permalink], @"values were nil");
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testGetApiTokenStatus {
    bool done = false;
    static NIKApiTokenStatus* target = nil;
    static NSError * gError = nil;
    
    [api getApiTokenStatusWithCompletionBlock:apiKey completionHandler:^(NIKApiTokenStatus *status, NSError *error) {
        if(error) {
            gError = error;
        }
        if(status == nil){
            NSLog(@"failed to get data");
        }
        else {
            target = [[NIKApiTokenStatus alloc] initWithValues:[status asDictionary]];
        }
    }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertNotNil([target token], @"token was nil");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}


- (void)testGetLoggedInUser {
    bool done = false;
    static NIKAuthenticationToken * auth = nil;
    static NIKUser* target = nil;
    static NSError * gError = nil;
    
    [api authenticatePostWithCompletionBlock:username
                                        body:password
                           completionHandler:^(NIKAuthenticationToken * authToken, NSError * error) {
                               if(error) {
                                   gError = error;
                               }
                               if(authToken == nil){
                                   NSLog(@"failed to get data");
                               }
                               else {
                                   auth = [[NIKAuthenticationToken alloc] initWithValues:[authToken asDictionary]];
                               }
                           }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(auth){
            STAssertNotNil([auth token], @"token was nil");
            done = true;
        }
    }
    
    done = false;
    [api getLoggedInUserWithCompletionBlock:[auth token] completionHandler:^(NIKUser *user, NSError *error) {
        if(error) {
            gError = error;
        }
        if(user == nil){
            NSLog(@"failed to get data");
        }
        else {
            target = [[NIKUser alloc] initWithValues:[user asDictionary]];
        }
    }];
    
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertNotNil([target userName], @"username was nil");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}
@end
