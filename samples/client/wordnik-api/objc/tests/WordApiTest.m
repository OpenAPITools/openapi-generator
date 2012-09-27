#import "WordApiTest.h"

@implementation WordApiTest

- (void)setUp {
    [super setUp];
    api = [[NIKWordApi alloc ]init];
    NSString * api_key = [[[NSProcessInfo processInfo] environment] objectForKey:@"API_KEY"];
    
    if(api_key == nil){
        STFail(@"API_KEY environment variable was not set");
    }
    [api addHeader:api_key forKey:@"api_key"];
}

- (void)tearDown {
    [super tearDown];
}

- (void)testVerifyWord {
    bool done = false;
    static NIKWordObject* target = nil;
    static NSError * gError = nil;
    
    [api getWordWithCompletionBlock:@"cat"
                       useCanonical:@"false"
                 includeSuggestions:@"true"
                  completionHandler:^(NIKWordObject* word, NSError* error) {
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
            STAssertEqualObjects(@"cat", [target word], @"words were not equal");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testGetWordWithSuggestions {
    bool done = false;
    static NIKWordObject* target = nil;
    static NSError * gError = nil;
    
    [api getWordWithCompletionBlock:@"cAt"
                       useCanonical:@"false"
                 includeSuggestions:@"true"
                  completionHandler:^(NIKWordObject* word, NSError* error) {
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
            STAssertEqualObjects(@"cAt", [target word], @"words were not equal");
            STAssertTrue([[target suggestions] count] > 0, @"wrong number of suggestions returned");
            STAssertEqualObjects(@"cat", [[target suggestions] objectAtIndex:0], @"suggested value was not correct");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testFetchWordWithCanonicalForm {
    bool done = false;
    static NIKWordObject* target = nil;
    static NSError * gError = nil;
    
    [api getWordWithCompletionBlock:@"cAt"
                       useCanonical:@"true"
                 includeSuggestions:@"true"
                  completionHandler:^(NIKWordObject* word, NSError* error) {
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
            STAssertEqualObjects(@"cat", [target word], @"canonical form was not equal");
            STAssertEqualObjects(@"cAt", [target originalWord], @"original word was not equal");
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testFetchDefinitions {
    bool done = false;
    static NSArray* target = nil;
    static NSError * gError = nil;
    
    [api getDefinitionsWithCompletionBlock:@"cat" partOfSpeech:nil
                        sourceDictionaries:nil
                                     limit:[NSNumber numberWithInt:10]
                            includeRelated:nil
                              useCanonical:nil
                               includeTags:nil
                         completionHandler:^(NSArray *definitions, NSError *error) {
        if(error) {
            gError = error;
        }
        if(definitions == nil){
            NSLog(@"failed to get data");
        }
        else {
            target = [[NSArray alloc ]initWithArray:definitions];
        }
    }];
        
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertTrue([target count] == 10, @"definition count was not correct");
            
            for(NIKDefinition* def in target){
                STAssertEqualObjects(@"cat", [def word], @"word was not correct");
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testFetchExamples {
    bool done = false;
    static NIKExampleSearchResults* target = nil;
    static NSError * gError = nil;
    
    [api getExamplesWithCompletionBlock:@"cat" includeDuplicates:@"false" useCanonical:@"false" skip:[NSNumber numberWithInt:0] limit:[NSNumber numberWithInt:5] completionHandler:
     ^(NIKExampleSearchResults* results, NSError* error) {
         if(error) {
             gError = error;
         }
         if(results == nil){
             NSLog(@"failed to get data");
         }
         else {
             target = [[NIKExampleSearchResults alloc] initWithValues:[results asDictionary]];
         }
     }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertTrue([[target examples] count] == 5, @"definition count was not correct");
            for(NIKExample * example in [target examples]){
                STAssertEqualObjects(@"cat", [example word], @"word was not correct");
                STAssertNotNil([example text], @"example text was not valid");
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testTopFetchExample {
    bool done = false;
    static NIKExample * target = nil;
    static NSError * gError = nil;
    
    [api getTopExampleWithCompletionBlock:@"cat" useCanonical:@"false" completionHandler:
     ^(NIKExample* example, NSError* error) {
         if(error) {
             gError = error;
         }
         if(example == nil){
             NSLog(@"failed to get data");
         }
         else {
             target = [[NIKExample alloc] initWithValues:[example asDictionary]];
         }
     }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertEqualObjects(@"cat", [target word], @"word was not correct");
            STAssertNotNil([target text], @"example text was not valid");
            NSLog(@"%@", [target asDictionary]);
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testFetchPronunciations {
    bool done = false;
    static NSArray * target = nil;
    static NSError * gError = nil;
    
    [api getTextPronunciationsWithCompletionBlock:@"cat"
                                 sourceDictionary:nil
                                       typeFormat:nil
                                     useCanonical:@"false"
                                            limit:[NSNumber numberWithInt:10]
                                completionHandler:^(NSArray * prons, NSError * error) {
        if(error) {
            gError = error;
        }
        if(prons == nil){
            NSLog(@"failed to get data");
        }
        else {
            target = [[NSArray alloc]initWithArray:prons];
        }
    }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            STAssertTrue([target count] >= 2, @"text pron count was not correct");
            for(NIKTextPron * pron in target) {
                NSLog(@"prons: %@", [pron asDictionary]);
                STAssertNotNil([pron raw], [pron rawType], @"missing raw or raw type fields");
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testFetchFrequency {
    bool done = false;
    static NIKFrequencySummary * target = nil;
    static NSError * gError = nil;
    
    [api getWordFrequencyWithCompletionBlock:@"cat"
                                useCanonical:@"false"
                                   startYear:nil
                                     endYear:nil
                           completionHandler:^(NIKFrequencySummary * summary, NSError* error) {
                               if(error) {
                                   gError = error;
                               }
                               if(summary == nil){
                                   NSLog(@"failed to get data");
                               }
                               else {
                                   target = [[NIKFrequencySummary alloc]initWithValues:[summary asDictionary]];
                               }
                           }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            for(NIKFrequency * frequency in [target frequency]){
                STAssertNotNil([frequency count], @"count was nil");
                STAssertNotNil([frequency year], @"year was nil");
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testGetPhrases {
    bool done = false;
    static NSArray * target = nil;
    static NSError * gError = nil;
    
    [api getPhrasesWithCompletionBlock:@"money"
                                 limit:[NSNumber numberWithInt:5]
                                  wlmi:nil
                          useCanonical:nil
                     completionHandler:^(NSArray * phrases, NSError * error) {
                         if(error) {
                             gError = error;
                         }
                         if(phrases == nil){
                             NSLog(@"failed to get data");
                         }
                         else {
                             target = [[NSArray alloc] initWithArray:phrases];
                         }
                     }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            for(NIKBigram * phrase in target){
                STAssertNotNil([phrase gram1], @"gram1 was nil");
                STAssertNotNil([phrase gram2], @"gram2 was nil");
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}

-(void) testGetRelatedWords {
    bool done = false;
    static NSArray * target = nil;
    static NSError * gError = nil;
    
    [api getRelatedWordsWithCompletionBlock:@"cat"
                          relationshipTypes:nil
                               useCanonical:nil
                   limitPerRelationshipType:[NSNumber numberWithInt:10]
                          completionHandler:^(NSArray * relateds, NSError * error) {
                              if(error) {
                                  gError = error;
                              }
                              if(relateds == nil){
                                  NSLog(@"failed to get data");
                              }
                              else {
                                  target = [[NSArray alloc] initWithArray:relateds];
                              }
                          }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            for(NIKRelated * rel in target){
                STAssertNotNil([rel relationshipType], @"relationship type was nil");
                STAssertTrue([[rel words] count] <= 10, @"Too many words returned");
                for(NSString * word in [rel words]){
                    STAssertNotNil(word, @"word was nil");
                }
                NSLog(@"%@", [rel asDictionary]);
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");
}


-(void) testGetAudio {
    bool done = false;
    static NSArray * target = nil;
    static NSError * gError = nil;
    
    [api getAudioWithCompletionBlock:@"cat" useCanonical:nil
                               limit:[NSNumber numberWithInt:2]
                   completionHandler:^(NSArray * audio, NSError *error) {
                       
                       if(error) {
                           gError = error;
                       }
                       if(audio == nil){
                           NSLog(@"failed to get data");
                       }
                       else {
                           target = [[NSArray alloc] initWithArray:audio];
                       }
                   }];
    
    NSDate * loopUntil = [NSDate dateWithTimeIntervalSinceNow:10];
    while(!done && [loopUntil timeIntervalSinceNow] > 0){
        if(gError){
            STFail(@"got error %@", gError);
            done = true;
        }
        if(target){
            for(NIKAudioFile * audio in target){
                STAssertNotNil([audio fileUrl], @"url was nil");
                STAssertEqualObjects([audio word], @"cat", @"word doesn't match");
                
                NSLog(@"%@", [audio asDictionary]);
            }
            done = true;
        }
    }
    STAssertNotNil(target, @"failed to fetch valid result in 10 seconds");   
}

@end
