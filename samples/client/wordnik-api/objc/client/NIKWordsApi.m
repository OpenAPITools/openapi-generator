#import "NIKWordsApi.h"
#import "NIKWordObject.h"
#import "NIKDefinitionSearchResults.h"
#import "NIKWordOfTheDay.h"
#import "NIKWordSearchResults.h"



@implementation NIKWordsApi
static NSString * basePath = @"http://api.wordnik.com/v4";

@synthesize queue = _queue;
@synthesize api = _api;

- (id) init {
    self = [super init];
    _queue = [[NSOperationQueue alloc] init];
    _api = [[NIKApiInvoker alloc] init];

    return self;
}

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key {
    [_api addHeader:value forKey:key];
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKWordSearchResults
 * returnContainer: 
 * 
 **/
-(void) searchWordsWithCompletionBlock :(NSString*) query includePartOfSpeech:(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech caseSensitive:(NSString*) caseSensitive minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NIKWordSearchResults*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/search/{query}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"query", @"}"]] withString: [_api escapeString:query]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(caseSensitive != nil)
        [queryParams setValue:caseSensitive forKey:@"caseSensitive"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minDictionaryCount != nil)
        [queryParams setValue:minDictionaryCount forKey:@"minDictionaryCount"];
    if(maxDictionaryCount != nil)
        [queryParams setValue:maxDictionaryCount forKey:@"maxDictionaryCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(query == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }
        
        completionBlock( [[NIKWordSearchResults alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKWordOfTheDay
 * returnContainer: 
 * 
 **/
-(void) getWordOfTheDayWithCompletionBlock :(NSString*) date 
        completionHandler:(void (^)(NIKWordOfTheDay*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/wordOfTheDay", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(date != nil)
        [queryParams setValue:date forKey:@"date"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }
        
        completionBlock( [[NIKWordOfTheDay alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKDefinitionSearchResults
 * returnContainer: 
 * 
 **/
-(void) reverseDictionaryWithCompletionBlock :(NSString*) query findSenseForWord:(NSString*) findSenseForWord includeSourceDictionaries:(NSString*) includeSourceDictionaries excludeSourceDictionaries:(NSString*) excludeSourceDictionaries includePartOfSpeech:(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech expandTerms:(NSString*) expandTerms sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength includeTags:(NSString*) includeTags skip:(NSString*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NIKDefinitionSearchResults*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/reverseDictionary", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(query != nil)
        [queryParams setValue:query forKey:@"query"];
    if(findSenseForWord != nil)
        [queryParams setValue:findSenseForWord forKey:@"findSenseForWord"];
    if(includeSourceDictionaries != nil)
        [queryParams setValue:includeSourceDictionaries forKey:@"includeSourceDictionaries"];
    if(excludeSourceDictionaries != nil)
        [queryParams setValue:excludeSourceDictionaries forKey:@"excludeSourceDictionaries"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    if(expandTerms != nil)
        [queryParams setValue:expandTerms forKey:@"expandTerms"];
    if(includeTags != nil)
        [queryParams setValue:includeTags forKey:@"includeTags"];
    if(sortBy != nil)
        [queryParams setValue:sortBy forKey:@"sortBy"];
    if(sortOrder != nil)
        [queryParams setValue:sortOrder forKey:@"sortOrder"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(query == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }
        
        completionBlock( [[NIKDefinitionSearchResults alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKWordObject
 * returnContainer: List
 * 
 **/
-(void) getRandomWordsWithCompletionBlock :(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder hasDictionaryDef:(NSString*) hasDictionaryDef minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/randomWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(hasDictionaryDef != nil)
        [queryParams setValue:hasDictionaryDef forKey:@"hasDictionaryDef"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minDictionaryCount != nil)
        [queryParams setValue:minDictionaryCount forKey:@"minDictionaryCount"];
    if(maxDictionaryCount != nil)
        [queryParams setValue:maxDictionaryCount forKey:@"maxDictionaryCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    if(sortBy != nil)
        [queryParams setValue:sortBy forKey:@"sortBy"];
    if(sortOrder != nil)
        [queryParams setValue:sortOrder forKey:@"sortOrder"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NIKWordObject* d = [[NIKWordObject alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKWordObject
 * returnContainer: 
 * 
 **/
-(void) getRandomWordWithCompletionBlock :(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech hasDictionaryDef:(NSString*) hasDictionaryDef minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength 
        completionHandler:(void (^)(NIKWordObject*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/randomWord", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(hasDictionaryDef != nil)
        [queryParams setValue:hasDictionaryDef forKey:@"hasDictionaryDef"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minDictionaryCount != nil)
        [queryParams setValue:minDictionaryCount forKey:@"minDictionaryCount"];
    if(maxDictionaryCount != nil)
        [queryParams setValue:maxDictionaryCount forKey:@"maxDictionaryCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }
        
        completionBlock( [[NIKWordObject alloc]initWithValues: data], nil);}];
    
}

-(void) searchWordsAsJsonWithCompletionBlock :(NSString*) query includePartOfSpeech:(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech caseSensitive:(NSString*) caseSensitive minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/search/{query}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"query", @"}"]] withString: [_api escapeString:query]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(caseSensitive != nil)
        [queryParams setValue:caseSensitive forKey:@"caseSensitive"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minDictionaryCount != nil)
        [queryParams setValue:minDictionaryCount forKey:@"minDictionaryCount"];
    if(maxDictionaryCount != nil)
        [queryParams setValue:maxDictionaryCount forKey:@"maxDictionaryCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(query == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        
    }];

}

-(void) getWordOfTheDayAsJsonWithCompletionBlock :(NSString*) date 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/wordOfTheDay", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(date != nil)
        [queryParams setValue:date forKey:@"date"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        
    }];

}

-(void) reverseDictionaryAsJsonWithCompletionBlock :(NSString*) query findSenseForWord:(NSString*) findSenseForWord includeSourceDictionaries:(NSString*) includeSourceDictionaries excludeSourceDictionaries:(NSString*) excludeSourceDictionaries includePartOfSpeech:(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech expandTerms:(NSString*) expandTerms sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength includeTags:(NSString*) includeTags skip:(NSString*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/reverseDictionary", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(query != nil)
        [queryParams setValue:query forKey:@"query"];
    if(findSenseForWord != nil)
        [queryParams setValue:findSenseForWord forKey:@"findSenseForWord"];
    if(includeSourceDictionaries != nil)
        [queryParams setValue:includeSourceDictionaries forKey:@"includeSourceDictionaries"];
    if(excludeSourceDictionaries != nil)
        [queryParams setValue:excludeSourceDictionaries forKey:@"excludeSourceDictionaries"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    if(expandTerms != nil)
        [queryParams setValue:expandTerms forKey:@"expandTerms"];
    if(includeTags != nil)
        [queryParams setValue:includeTags forKey:@"includeTags"];
    if(sortBy != nil)
        [queryParams setValue:sortBy forKey:@"sortBy"];
    if(sortOrder != nil)
        [queryParams setValue:sortOrder forKey:@"sortOrder"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(query == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        
    }];

}

-(void) getRandomWordsAsJsonWithCompletionBlock :(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder hasDictionaryDef:(NSString*) hasDictionaryDef minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/randomWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(hasDictionaryDef != nil)
        [queryParams setValue:hasDictionaryDef forKey:@"hasDictionaryDef"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minDictionaryCount != nil)
        [queryParams setValue:minDictionaryCount forKey:@"minDictionaryCount"];
    if(maxDictionaryCount != nil)
        [queryParams setValue:maxDictionaryCount forKey:@"maxDictionaryCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    if(sortBy != nil)
        [queryParams setValue:sortBy forKey:@"sortBy"];
    if(sortOrder != nil)
        [queryParams setValue:sortOrder forKey:@"sortOrder"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        
    }];

}

-(void) getRandomWordAsJsonWithCompletionBlock :(NSString*) includePartOfSpeech excludePartOfSpeech:(NSString*) excludePartOfSpeech hasDictionaryDef:(NSString*) hasDictionaryDef minCorpusCount:(NSNumber*) minCorpusCount maxCorpusCount:(NSNumber*) maxCorpusCount minDictionaryCount:(NSNumber*) minDictionaryCount maxDictionaryCount:(NSNumber*) maxDictionaryCount minLength:(NSNumber*) minLength maxLength:(NSNumber*) maxLength 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.{format}/randomWord", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(hasDictionaryDef != nil)
        [queryParams setValue:hasDictionaryDef forKey:@"hasDictionaryDef"];
    if(includePartOfSpeech != nil)
        [queryParams setValue:includePartOfSpeech forKey:@"includePartOfSpeech"];
    if(excludePartOfSpeech != nil)
        [queryParams setValue:excludePartOfSpeech forKey:@"excludePartOfSpeech"];
    if(minCorpusCount != nil)
        [queryParams setValue:minCorpusCount forKey:@"minCorpusCount"];
    if(maxCorpusCount != nil)
        [queryParams setValue:maxCorpusCount forKey:@"maxCorpusCount"];
    if(minDictionaryCount != nil)
        [queryParams setValue:minDictionaryCount forKey:@"minDictionaryCount"];
    if(maxDictionaryCount != nil)
        [queryParams setValue:maxDictionaryCount forKey:@"maxDictionaryCount"];
    if(minLength != nil)
        [queryParams setValue:minLength forKey:@"minLength"];
    if(maxLength != nil)
        [queryParams setValue:maxLength forKey:@"maxLength"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"GET" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        
    }];

}


@end
