#import "WordListApiTest.h"

@implementation WordListApiTest

- (void)setUp {
    [super setUp];
    api = [[NIKWordListApi alloc ]init];
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
    
    NIKAccountApi * accountApi = [[NIKAccountApi alloc] init];
    [accountApi addHeader:apiKey forKey:@"api_key"];
    
    bool done = false;
    [accountApi authenticatePostWithCompletionBlock:username
                                               body:password
                                  completionHandler:^(NIKAuthenticationToken * l, NSError *error) {
                                      auth = [[NIKAuthenticationToken alloc] initWithValues:[l asDictionary]];
                                  }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(auth) {
            done = true;
        }
    }
    STAssertNotNil(auth, @"auth token was nil");
    
    // create sample list to work with in test
    listsApi = [[NIKWordListsApi alloc] init];
    [listsApi addHeader:apiKey forKey:@"api_key"];
    
    NIKWordList * list = [[NIKWordList alloc] init];
    list.name = @"my test list";
    list.type = @"PUBLIC";
    list.description = @"some words I want to play with";
    
    [listsApi createWordListWithCompletionBlock:list
                                     auth_token:auth.token
                              completionHandler:^(NIKWordList *responseList, NSError *error) {
                                  sampleList = [[NIKWordList alloc] initWithValues:[responseList asDictionary]];
                                  if(error) {
                                      NSLog(@"%@", error);
                                      STFail(@"error making sample list");
                                  }
                                  
                                  NSLog(@"created sample list %@", [sampleList asDictionary]);
                              }];
    
    done = false;
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(sampleList) {
            done = true;
            NSLog(@"created sample list %@", [sampleList asDictionary]);
        }
    }
}

- (void)tearDown {
    [super tearDown];
    [api deleteWordListWithCompletionBlock:[sampleList permalink]
                                auth_token:[auth token] completionHandler:^(NSError *error) {
                                    if(error) {
                                        NSLog(@"%@", error);
                                        STFail(@"error making sample list");
                                    }
                                    
                                }];
}

- (void)testGetWordListByPermalink {
    bool done = false;
    static NIKWordList* target = nil;
    static NSError * gError = nil;
    
    [api getWordListByPermalinkWithCompletionBlock:[sampleList permalink]
                                        auth_token:[auth token]
                                 completionHandler:^(NIKWordList * list, NSError *error) {
                                     if(error) {
                                         gError = error;
                                     }
                                     if(list == nil){
                                         NSLog(@"failed to get data");
                                     }
                                     else {
                                         target = [[NIKWordList alloc] initWithValues:[list asDictionary]];
                                     }
                                     
                                 }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertEqualObjects([sampleList permalink] , [target permalink], @"response list has bad permalink");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testUpdateWordList {
    bool done = false;
    static NIKWordList* target = nil;
    static NSError * gError = nil;
    
    [api getWordListByPermalinkWithCompletionBlock:[sampleList permalink]
                                        auth_token:[auth token]
                                 completionHandler:^(NIKWordList * list, NSError *error) {
                                     if(error) {
                                         gError = error;
                                     }
                                     if(list == nil){
                                         NSLog(@"failed to get data");
                                     }
                                     else {
                                         target = [[NIKWordList alloc] initWithValues:[list asDictionary]];
                                     }
                                 }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
    
    // save the list
    done = false;
    NSDate * date = [[NSDate alloc] init];
    NSDateFormatter* dateFormatter = [[NSDateFormatter alloc] init];
    [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm:ss"];
    NSString *dateString = [dateFormatter stringFromDate:[NSDate date]];
    NSString * updatedDescription = [NSString stringWithFormat:@"list updated at %@", dateString];
    
    target.description = updatedDescription;
    [api updateWordListWithCompletionBlock:[sampleList permalink]
                                      body:target
                                auth_token:[auth token]
                         completionHandler:^(NSError * error) {
                             if(error) {
                                 gError = error;
                             }
                         }];
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            done = true;
        }
    }
    
    // fetch & verify again
    [api getWordListByPermalinkWithCompletionBlock:[sampleList permalink]
                                        auth_token:[auth token]
                                 completionHandler:^(NIKWordList * list, NSError *error) {
                                     if(error) {
                                         gError = error;
                                     }
                                     if(list == nil){
                                         NSLog(@"failed to get data");
                                     }
                                     else {
                                         target = [[NIKWordList alloc] initWithValues:[list asDictionary]];
                                     }
                                 }];
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            done = true;
        }
    }
    
    STAssertEqualObjects(updatedDescription, [target description], @"failed to verify description was updated");
}

- (void)testAddWordsToWordList {
    bool done = false;
    static NIKWordList* target = nil;
    static NSError * gError = nil;
    
    [api getWordListByPermalinkWithCompletionBlock:[sampleList permalink]
                                        auth_token:[auth token]
                                 completionHandler:^(NIKWordList * list, NSError *error) {
                                     if(error) {
                                         gError = error;
                                     }
                                     if(list == nil){
                                         NSLog(@"failed to get data");
                                     }
                                     else {
                                         target = [[NIKWordList alloc] initWithValues:[list asDictionary]];
                                     }
                                     
                                 }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
    
    done = false;
    
    // add words
    NSMutableArray * wordsToAdd = [[NSMutableArray alloc] init];
    NIKWordListWord * word = [[NIKWordListWord alloc] init];
    word.word = @"delicious";
    [wordsToAdd addObject:word];
    NSLog(@"is swagger object? %@", [word respondsToSelector:@selector(asDictionary:)]);
    
    word = [[NIKWordListWord alloc] init];
    word.word = @"tasty";
    [wordsToAdd addObject:word];
    
    word = [[NIKWordListWord alloc] init];
    word.word = @"scrumptious";
    [wordsToAdd addObject:word];
    
    static bool hasResponse = false;
    
    [api addWordsToWordListWithCompletionBlock: [sampleList permalink]
                                          body: [[NSArray alloc] initWithArray:wordsToAdd]
                                    auth_token: [auth token]
                             completionHandler:^(NSError *error) {
                                 if(error){
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
        if(hasResponse){
            done = true;
        }
    }
    
    // verify they're added
    static NSArray * wordsInList = nil;
    done = false;
    [api getWordListWordsWithCompletionBlock:[sampleList permalink]
                                  auth_token:[auth token]
                                      sortBy:nil
                                   sortOrder:nil
                                        skip:nil
                                       limit:nil
                           completionHandler:^(NSArray *w, NSError *error) {
                               if(error) {
                                   gError = error;
                               }
                               else {
                                   wordsInList = w;
                               }
                           }];
    loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            done = true;
        }
    }
    
    for(NIKWordListWord * w in wordsInList){
        NSLog(@"word: %@", [w asDictionary]);
    }
}


@end
