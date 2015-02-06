#import "SWGWordApi.h"
#import "SWGFile.h"
#import "SWGApiClient.h"



@implementation SWGWordApi
static NSString * basePath = @"http://api.wordnik.com/v4";

+(SWGWordApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key {
    static SWGWordApi* singletonAPI = nil;

    if (singletonAPI == nil) {
        singletonAPI = [[SWGWordApi alloc] init];
        [singletonAPI addHeader:headerValue forKey:key];
    }
    return singletonAPI;
}

+(void) setBasePath:(NSString*)path {
    basePath = path;
}

+(NSString*) getBasePath {
    return basePath;
}

-(SWGApiClient*) apiClient {
    return [SWGApiClient sharedClientFromPool:basePath];
}

-(void) addHeader:(NSString*)value forKey:(NSString*)key {
    [[self apiClient] setHeaderValue:value forKey:key];
}

-(id) init {
    self = [super init];
    [self apiClient];
    return self;
}

-(void) setHeaderValue:(NSString*) value
           forKey:(NSString*)key {
    [[self apiClient] setHeaderValue:value forKey:key];
}

-(unsigned long) requestQueueSize {
    return [SWGApiClient requestQueueSize];
}


-(NSNumber*) getWordWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical         includeSuggestions:(NSString*) includeSuggestions        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(includeSuggestions != nil)
        queryParams[@"includeSuggestions"] = includeSuggestions;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getAudioWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical         limit:(NSNumber*) limit        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/audio", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getDefinitionsWithCompletionBlock:(NSString*) word         limit:(NSNumber*) limit         partOfSpeech:(NSString*) partOfSpeech         includeRelated:(NSString*) includeRelated         sourceDictionaries:(NSArray*) sourceDictionaries         useCanonical:(NSString*) useCanonical         includeTags:(NSString*) includeTags        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/definitions", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(limit != nil)
        queryParams[@"limit"] = limit;
    if(partOfSpeech != nil)
        queryParams[@"partOfSpeech"] = partOfSpeech;
    if(includeRelated != nil)
        queryParams[@"includeRelated"] = includeRelated;
    if(sourceDictionaries != nil)
        queryParams[@"sourceDictionaries"] = sourceDictionaries;
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(includeTags != nil)
        queryParams[@"includeTags"] = includeTags;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getEtymologiesWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/etymologies", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getExamplesWithCompletionBlock:(NSString*) word         includeDuplicates:(NSString*) includeDuplicates         useCanonical:(NSString*) useCanonical         skip:(NSNumber*) skip         limit:(NSNumber*) limit        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/examples", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(includeDuplicates != nil)
        queryParams[@"includeDuplicates"] = includeDuplicates;
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(skip != nil)
        queryParams[@"skip"] = skip;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getWordFrequencyWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical         startYear:(NSNumber*) startYear         endYear:(NSNumber*) endYear        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/frequency", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(startYear != nil)
        queryParams[@"startYear"] = startYear;
    if(endYear != nil)
        queryParams[@"endYear"] = endYear;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getHyphenationWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical         sourceDictionary:(NSString*) sourceDictionary         limit:(NSNumber*) limit        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/hyphenation", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(sourceDictionary != nil)
        queryParams[@"sourceDictionary"] = sourceDictionary;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getPhrasesWithCompletionBlock:(NSString*) word         limit:(NSNumber*) limit         wlmi:(NSNumber*) wlmi         useCanonical:(NSString*) useCanonical        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/phrases", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(limit != nil)
        queryParams[@"limit"] = limit;
    if(wlmi != nil)
        queryParams[@"wlmi"] = wlmi;
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getTextPronunciationsWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical         sourceDictionary:(NSString*) sourceDictionary         typeFormat:(NSString*) typeFormat         limit:(NSNumber*) limit        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/pronunciations", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(sourceDictionary != nil)
        queryParams[@"sourceDictionary"] = sourceDictionary;
    if(typeFormat != nil)
        queryParams[@"typeFormat"] = typeFormat;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getRelatedWordsWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical         relationshipTypes:(NSString*) relationshipTypes         limitPerRelationshipType:(NSNumber*) limitPerRelationshipType        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/relatedWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    if(relationshipTypes != nil)
        queryParams[@"relationshipTypes"] = relationshipTypes;
    if(limitPerRelationshipType != nil)
        queryParams[@"limitPerRelationshipType"] = limitPerRelationshipType;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}

-(NSNumber*) getTopExampleWithCompletionBlock:(NSString*) word         useCanonical:(NSString*) useCanonical        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.json/{word}/topExample", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [SWGApiClient escape:word]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

        NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        queryParams[@"useCanonical"] = useCanonical;
    
        NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
        
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    
    
    
    return [client stringWithCompletionBlock:requestUrl 
                                             method:@"GET" 
                                        queryParams:queryParams 
                                               body:bodyDictionary 
                                       headerParams:headerParams
                                 requestContentType: requestContentType
                                responseContentType: responseContentType
                                    completionBlock:^(NSString *data, NSError *error) {
                        if (error) {
                            completionBlock(error);
                            return;
                        }
                        completionBlock(nil);
                    }];
    
    
    
    

}



@end