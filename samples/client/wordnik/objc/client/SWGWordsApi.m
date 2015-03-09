#import "SWGWordsApi.h"
#import "SWGFile.h"
#import "SWGApiClient.h"
#import "SWGWordObject.h"
#import "SWGDefinitionSearchResults.h"
#import "SWGWordSearchResults.h"
#import "SWGWordOfTheDay.h"



@implementation SWGWordsApi
static NSString * basePath = @"https://api.wordnik.com/v4";

+(SWGWordsApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key {
    static SWGWordsApi* singletonAPI = nil;

    if (singletonAPI == nil) {
        singletonAPI = [[SWGWordsApi alloc] init];
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


-(NSNumber*) getRandomWordWithCompletionBlock:(NSString*) hasDictionaryDef
         includePartOfSpeech:(NSString*) includePartOfSpeech
         excludePartOfSpeech:(NSString*) excludePartOfSpeech
         minCorpusCount:(NSNumber*) minCorpusCount
         maxCorpusCount:(NSNumber*) maxCorpusCount
         minDictionaryCount:(NSNumber*) minDictionaryCount
         maxDictionaryCount:(NSNumber*) maxDictionaryCount
         minLength:(NSNumber*) minLength
         maxLength:(NSNumber*) maxLength
        
        completionHandler: (void (^)(SWGWordObject* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.json/randomWord", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(hasDictionaryDef != nil)
        queryParams[@"hasDictionaryDef"] = hasDictionaryDef;
    if(includePartOfSpeech != nil)
        queryParams[@"includePartOfSpeech"] = includePartOfSpeech;
    if(excludePartOfSpeech != nil)
        queryParams[@"excludePartOfSpeech"] = excludePartOfSpeech;
    if(minCorpusCount != nil)
        queryParams[@"minCorpusCount"] = minCorpusCount;
    if(maxCorpusCount != nil)
        queryParams[@"maxCorpusCount"] = maxCorpusCount;
    if(minDictionaryCount != nil)
        queryParams[@"minDictionaryCount"] = minDictionaryCount;
    if(maxDictionaryCount != nil)
        queryParams[@"maxDictionaryCount"] = maxDictionaryCount;
    if(minLength != nil)
        queryParams[@"minLength"] = minLength;
    if(maxLength != nil)
        queryParams[@"maxLength"] = maxLength;
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
    
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    
    
        
    // comples response type
    return [client dictionary:requestUrl 
                       method:@"GET" 
                  queryParams:queryParams 
                         body:bodyDictionary 
                 headerParams:headerParams
           requestContentType:requestContentType
          responseContentType:responseContentType
              completionBlock:^(NSDictionary *data, NSError *error) {
                if (error) {
                    completionBlock(nil, error);
                    
                    return;
                }
                
                SWGWordObject *result = nil;
                if (data) {
                    result = [[SWGWordObject    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}

-(NSNumber*) getRandomWordsWithCompletionBlock:(NSString*) hasDictionaryDef
         includePartOfSpeech:(NSString*) includePartOfSpeech
         excludePartOfSpeech:(NSString*) excludePartOfSpeech
         minCorpusCount:(NSNumber*) minCorpusCount
         maxCorpusCount:(NSNumber*) maxCorpusCount
         minDictionaryCount:(NSNumber*) minDictionaryCount
         maxDictionaryCount:(NSNumber*) maxDictionaryCount
         minLength:(NSNumber*) minLength
         maxLength:(NSNumber*) maxLength
         sortBy:(NSString*) sortBy
         sortOrder:(NSString*) sortOrder
         limit:(NSNumber*) limit
        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.json/randomWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(hasDictionaryDef != nil)
        queryParams[@"hasDictionaryDef"] = hasDictionaryDef;
    if(includePartOfSpeech != nil)
        queryParams[@"includePartOfSpeech"] = includePartOfSpeech;
    if(excludePartOfSpeech != nil)
        queryParams[@"excludePartOfSpeech"] = excludePartOfSpeech;
    if(minCorpusCount != nil)
        queryParams[@"minCorpusCount"] = minCorpusCount;
    if(maxCorpusCount != nil)
        queryParams[@"maxCorpusCount"] = maxCorpusCount;
    if(minDictionaryCount != nil)
        queryParams[@"minDictionaryCount"] = minDictionaryCount;
    if(maxDictionaryCount != nil)
        queryParams[@"maxDictionaryCount"] = maxDictionaryCount;
    if(minLength != nil)
        queryParams[@"minLength"] = minLength;
    if(maxLength != nil)
        queryParams[@"maxLength"] = maxLength;
    if(sortBy != nil)
        queryParams[@"sortBy"] = sortBy;
    if(sortOrder != nil)
        queryParams[@"sortOrder"] = sortOrder;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
    
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    
            // primitive response type
    
    
    // no return base type
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

-(NSNumber*) reverseDictionaryWithCompletionBlock:(NSString*) query
         findSenseForWord:(NSString*) findSenseForWord
         includeSourceDictionaries:(NSString*) includeSourceDictionaries
         excludeSourceDictionaries:(NSString*) excludeSourceDictionaries
         includePartOfSpeech:(NSString*) includePartOfSpeech
         excludePartOfSpeech:(NSString*) excludePartOfSpeech
         minCorpusCount:(NSNumber*) minCorpusCount
         maxCorpusCount:(NSNumber*) maxCorpusCount
         minLength:(NSNumber*) minLength
         maxLength:(NSNumber*) maxLength
         expandTerms:(NSString*) expandTerms
         includeTags:(NSString*) includeTags
         sortBy:(NSString*) sortBy
         sortOrder:(NSString*) sortOrder
         skip:(NSString*) skip
         limit:(NSNumber*) limit
        
        completionHandler: (void (^)(SWGDefinitionSearchResults* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.json/reverseDictionary", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(query != nil)
        queryParams[@"query"] = query;
    if(findSenseForWord != nil)
        queryParams[@"findSenseForWord"] = findSenseForWord;
    if(includeSourceDictionaries != nil)
        queryParams[@"includeSourceDictionaries"] = includeSourceDictionaries;
    if(excludeSourceDictionaries != nil)
        queryParams[@"excludeSourceDictionaries"] = excludeSourceDictionaries;
    if(includePartOfSpeech != nil)
        queryParams[@"includePartOfSpeech"] = includePartOfSpeech;
    if(excludePartOfSpeech != nil)
        queryParams[@"excludePartOfSpeech"] = excludePartOfSpeech;
    if(minCorpusCount != nil)
        queryParams[@"minCorpusCount"] = minCorpusCount;
    if(maxCorpusCount != nil)
        queryParams[@"maxCorpusCount"] = maxCorpusCount;
    if(minLength != nil)
        queryParams[@"minLength"] = minLength;
    if(maxLength != nil)
        queryParams[@"maxLength"] = maxLength;
    if(expandTerms != nil)
        queryParams[@"expandTerms"] = expandTerms;
    if(includeTags != nil)
        queryParams[@"includeTags"] = includeTags;
    if(sortBy != nil)
        queryParams[@"sortBy"] = sortBy;
    if(sortOrder != nil)
        queryParams[@"sortOrder"] = sortOrder;
    if(skip != nil)
        queryParams[@"skip"] = skip;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
    
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    
    
        
    // comples response type
    return [client dictionary:requestUrl 
                       method:@"GET" 
                  queryParams:queryParams 
                         body:bodyDictionary 
                 headerParams:headerParams
           requestContentType:requestContentType
          responseContentType:responseContentType
              completionBlock:^(NSDictionary *data, NSError *error) {
                if (error) {
                    completionBlock(nil, error);
                    
                    return;
                }
                
                SWGDefinitionSearchResults *result = nil;
                if (data) {
                    result = [[SWGDefinitionSearchResults    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}

-(NSNumber*) searchWordsWithCompletionBlock:(NSString*) query
         caseSensitive:(NSString*) caseSensitive
         includePartOfSpeech:(NSString*) includePartOfSpeech
         excludePartOfSpeech:(NSString*) excludePartOfSpeech
         minCorpusCount:(NSNumber*) minCorpusCount
         maxCorpusCount:(NSNumber*) maxCorpusCount
         minDictionaryCount:(NSNumber*) minDictionaryCount
         maxDictionaryCount:(NSNumber*) maxDictionaryCount
         minLength:(NSNumber*) minLength
         maxLength:(NSNumber*) maxLength
         skip:(NSNumber*) skip
         limit:(NSNumber*) limit
        
        completionHandler: (void (^)(SWGWordSearchResults* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.json/search/{query}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"query", @"}"]] withString: [SWGApiClient escape:query]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(caseSensitive != nil)
        queryParams[@"caseSensitive"] = caseSensitive;
    if(includePartOfSpeech != nil)
        queryParams[@"includePartOfSpeech"] = includePartOfSpeech;
    if(excludePartOfSpeech != nil)
        queryParams[@"excludePartOfSpeech"] = excludePartOfSpeech;
    if(minCorpusCount != nil)
        queryParams[@"minCorpusCount"] = minCorpusCount;
    if(maxCorpusCount != nil)
        queryParams[@"maxCorpusCount"] = maxCorpusCount;
    if(minDictionaryCount != nil)
        queryParams[@"minDictionaryCount"] = minDictionaryCount;
    if(maxDictionaryCount != nil)
        queryParams[@"maxDictionaryCount"] = maxDictionaryCount;
    if(minLength != nil)
        queryParams[@"minLength"] = minLength;
    if(maxLength != nil)
        queryParams[@"maxLength"] = maxLength;
    if(skip != nil)
        queryParams[@"skip"] = skip;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
    
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    
    
        
    // comples response type
    return [client dictionary:requestUrl 
                       method:@"GET" 
                  queryParams:queryParams 
                         body:bodyDictionary 
                 headerParams:headerParams
           requestContentType:requestContentType
          responseContentType:responseContentType
              completionBlock:^(NSDictionary *data, NSError *error) {
                if (error) {
                    completionBlock(nil, error);
                    
                    return;
                }
                
                SWGWordSearchResults *result = nil;
                if (data) {
                    result = [[SWGWordSearchResults    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}

-(NSNumber*) getWordOfTheDayWithCompletionBlock:(NSString*) date
        
        completionHandler: (void (^)(SWGWordOfTheDay* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/words.json/wordOfTheDay", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(date != nil)
        queryParams[@"date"] = date;
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
    
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    
    
        
    // comples response type
    return [client dictionary:requestUrl 
                       method:@"GET" 
                  queryParams:queryParams 
                         body:bodyDictionary 
                 headerParams:headerParams
           requestContentType:requestContentType
          responseContentType:responseContentType
              completionBlock:^(NSDictionary *data, NSError *error) {
                if (error) {
                    completionBlock(nil, error);
                    
                    return;
                }
                
                SWGWordOfTheDay *result = nil;
                if (data) {
                    result = [[SWGWordOfTheDay    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}



@end