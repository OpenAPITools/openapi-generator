#import "WordsApiTest.h"

@implementation WordsApiTest
- (void)setUp {
    [super setUp];
    api = [[NIKWordsApi alloc ]init];
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

- (void)testSearchWords {
    bool done = false;
    static NIKWordSearchResults* target = nil;
    static NSError * gError = nil;
    
    [api searchWordsWithCompletionBlock:@"ca"
                    includePartOfSpeech:nil
                    excludePartOfSpeech:nil
                          caseSensitive:nil
                         minCorpusCount:nil
                         maxCorpusCount:nil
                     minDictionaryCount:nil
                     maxDictionaryCount:nil
                              minLength:nil
                              maxLength:nil
                                   skip:nil
                                  limit:nil
                      completionHandler:^(NIKWordSearchResults *results, NSError *error) {
        if(error) {
            gError = error;
        }
        if(results == nil){
            NSLog(@"failed to get data");
        }
        else {
            target = [[NIKWordSearchResults alloc] initWithValues:[results asDictionary]];
        }
    }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertTrue([[target searchResults] count] > 0, @"didn't get right number of results");
            NIKWordSearchResult * word = [[target searchResults] objectAtIndex:0];
            STAssertEqualObjects([word word], @"ca", @"didn't get expected first result");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testGetWordOfTheDay {
    bool done = false;
    static NIKWordOfTheDay* target = nil;
    static NSError * gError = nil;
    
    [api getWordOfTheDayWithCompletionBlock:nil completionHandler:^(NIKWordOfTheDay *wotd, NSError *error) {
        if(error) {
            gError = error;
        }
        if(wotd == nil){
            NSLog(@"failed to get data");
        }
        else {
            target = [[NIKWordOfTheDay alloc] initWithValues:[wotd asDictionary]];
        }
    }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            NSDateComponents *components = [[NSCalendar currentCalendar]
                                            components:NSDayCalendarUnit | NSMonthCalendarUnit | NSYearCalendarUnit fromDate:[[target publishDate] date]];
            
            int year = [components year];
            
            STAssertTrue(year <= 2012, @"wrong year for WOTD");
            STAssertNotNil([target word], @"word was missing");
            STAssertNotNil([target publishDate], @"creation date missing");
            STAssertTrue([[target publishDate] isKindOfClass:[NIKDate class]], @"publish date was the wrong type of object");
            
            STAssertTrue([[target definitions] count] > 0, @"missing definitions");
            STAssertTrue([[target examples] count] > 0, @"missing examples");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testGetReverseDictionary {
    bool done = false;
    static NIKDefinitionSearchResults* target = nil;
    static NSError * gError = nil;
    [api reverseDictionaryWithCompletionBlock:@"hairy"
                             findSenseForWord:nil
                    includeSourceDictionaries:nil
                    excludeSourceDictionaries:nil
                          includePartOfSpeech:nil
                          excludePartOfSpeech:nil
                                  expandTerms:nil
                                       sortBy:nil
                                    sortOrder:nil
                               minCorpusCount:nil
                               maxCorpusCount:nil
                                    minLength:nil
                                    maxLength:nil
                                  includeTags:nil
                                         skip:nil
                                        limit:nil
                            completionHandler:^(NIKDefinitionSearchResults *results, NSError *error) {
                                if(error) {
                                    gError = error;
                                }
                                if(results == nil){
                                    NSLog(@"failed to get data");
                                }
                                else {
                                    target = [[NIKDefinitionSearchResults alloc] initWithValues:[results asDictionary]];
                                }
                            }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertTrue([target totalResults] > 0, @"wrong total results");
            STAssertTrue([target results] > 0, @"wrong number of results");
            
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testGetRandomWords {
    bool done = false;
    static NSArray* target = nil;
    static NSError * gError = nil;
    
    [api getRandomWordsWithCompletionBlock:nil
                       excludePartOfSpeech:nil
                                    sortBy:nil
                                 sortOrder:nil
                          hasDictionaryDef:nil
                            minCorpusCount:nil
                            maxCorpusCount:nil
                        minDictionaryCount:nil
                        maxDictionaryCount:nil
                                 minLength:nil
                                 maxLength:nil
                                     limit:[NSNumber numberWithInt:10]
                         completionHandler:^(NSArray *words, NSError *error) {
                             if(error) {
                                 gError = error;
                             }
                             if(words == nil){
                                 NSLog(@"failed to get data");
                             }
                             else {
                                 target = [[NSArray alloc] initWithArray:words];
                             }
                         }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertTrue([target count] == 10, @"wrong number of words returned");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

- (void)testGetRandomWord {
    bool done = false;
    static NIKWordObject* target = nil;
    static NSError * gError = nil;

    [api  getRandomWordWithCompletionBlock:nil
                       excludePartOfSpeech:nil
                          hasDictionaryDef:nil
                            minCorpusCount:nil
                            maxCorpusCount:nil
                        minDictionaryCount:nil
                        maxDictionaryCount:nil
                                 minLength:nil
                                 maxLength:nil
                        completionHandler:^(NIKWordObject *word, NSError *error) {
                            if(error) {
                                gError = error;
                            }
                            if(word == nil){
                                NSLog(@"failed to get data");
                            }
                            else {
                                target = [[NIKWordObject alloc] initWithValues:[word asDictionary]];
                            }
                        }];
    
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertNotNil([target word], @"target word was nil");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}
@end
