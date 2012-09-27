#import "NIKWordApi.h"
#import "NIKDefinition.h"
#import "NIKTextPron.h"
#import "NIKExample.h"
#import "NIKSyllable.h"
#import "NIKAudioFile.h"
#import "NIKExampleSearchResults.h"
#import "NIKWordObject.h"
#import "NIKBigram.h"
#import "NIKRelated.h"
#import "NIKFrequencySummary.h"



@implementation NIKWordApi
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
 * returnBaseType: NIKExampleSearchResults
 * returnContainer: 
 * 
 **/
-(void) getExamplesWithCompletionBlock :(NSString*) word includeDuplicates:(NSString*) includeDuplicates useCanonical:(NSString*) useCanonical skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NIKExampleSearchResults*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/examples", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(includeDuplicates != nil)
        [queryParams setValue:includeDuplicates forKey:@"includeDuplicates"];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        completionBlock( [[NIKExampleSearchResults alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKWordObject
 * returnContainer: 
 * 
 **/
-(void) getWordWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical includeSuggestions:(NSString*) includeSuggestions 
        completionHandler:(void (^)(NIKWordObject*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(includeSuggestions != nil)
        [queryParams setValue:includeSuggestions forKey:@"includeSuggestions"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        completionBlock( [[NIKWordObject alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKDefinition
 * returnContainer: List
 * 
 **/
-(void) getDefinitionsWithCompletionBlock :(NSString*) word partOfSpeech:(NSString*) partOfSpeech sourceDictionaries:(NSString*) sourceDictionaries limit:(NSNumber*) limit includeRelated:(NSString*) includeRelated useCanonical:(NSString*) useCanonical includeTags:(NSString*) includeTags 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/definitions", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    if(partOfSpeech != nil)
        [queryParams setValue:partOfSpeech forKey:@"partOfSpeech"];
    if(includeRelated != nil)
        [queryParams setValue:includeRelated forKey:@"includeRelated"];
    if(sourceDictionaries != nil)
        [queryParams setValue:sourceDictionaries forKey:@"sourceDictionaries"];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(includeTags != nil)
        [queryParams setValue:includeTags forKey:@"includeTags"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NIKDefinition* d = [[NIKDefinition alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKExample
 * returnContainer: 
 * 
 **/
-(void) getTopExampleWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NIKExample*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/topExample", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        completionBlock( [[NIKExample alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKRelated
 * returnContainer: List
 * 
 **/
-(void) getRelatedWordsWithCompletionBlock :(NSString*) word relationshipTypes:(NSString*) relationshipTypes useCanonical:(NSString*) useCanonical limitPerRelationshipType:(NSNumber*) limitPerRelationshipType 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/relatedWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(relationshipTypes != nil)
        [queryParams setValue:relationshipTypes forKey:@"relationshipTypes"];
    if(limitPerRelationshipType != nil)
        [queryParams setValue:limitPerRelationshipType forKey:@"limitPerRelationshipType"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NIKRelated* d = [[NIKRelated alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKTextPron
 * returnContainer: List
 * 
 **/
-(void) getTextPronunciationsWithCompletionBlock :(NSString*) word sourceDictionary:(NSString*) sourceDictionary typeFormat:(NSString*) typeFormat useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/pronunciations", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(sourceDictionary != nil)
        [queryParams setValue:sourceDictionary forKey:@"sourceDictionary"];
    if(typeFormat != nil)
        [queryParams setValue:typeFormat forKey:@"typeFormat"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NIKTextPron* d = [[NIKTextPron alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKSyllable
 * returnContainer: List
 * 
 **/
-(void) getHyphenationWithCompletionBlock :(NSString*) word sourceDictionary:(NSString*) sourceDictionary useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/hyphenation", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(sourceDictionary != nil)
        [queryParams setValue:sourceDictionary forKey:@"sourceDictionary"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NIKSyllable* d = [[NIKSyllable alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKFrequencySummary
 * returnContainer: 
 * 
 **/
-(void) getWordFrequencyWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical startYear:(NSNumber*) startYear endYear:(NSNumber*) endYear 
        completionHandler:(void (^)(NIKFrequencySummary*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/frequency", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(startYear != nil)
        [queryParams setValue:startYear forKey:@"startYear"];
    if(endYear != nil)
        [queryParams setValue:endYear forKey:@"endYear"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        completionBlock( [[NIKFrequencySummary alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKBigram
 * returnContainer: List
 * 
 **/
-(void) getPhrasesWithCompletionBlock :(NSString*) word limit:(NSNumber*) limit wlmi:(NSNumber*) wlmi useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/phrases", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    if(wlmi != nil)
        [queryParams setValue:wlmi forKey:@"wlmi"];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NIKBigram* d = [[NIKBigram alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: true
 * returnBaseType: NSString
 * returnContainer: List
 * 
 **/
-(void) getEtymologiesWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/etymologies", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NSString* d = [[NSString alloc]initWithString: data];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKAudioFile
 * returnContainer: List
 * 
 **/
-(void) getAudioWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/audio", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(word == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in (NSArray*)data) {
                NIKAudioFile* d = [[NIKAudioFile alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

-(void) getExamplesAsJsonWithCompletionBlock :(NSString*) word includeDuplicates:(NSString*) includeDuplicates useCanonical:(NSString*) useCanonical skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/examples", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(includeDuplicates != nil)
        [queryParams setValue:includeDuplicates forKey:@"includeDuplicates"];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getWordAsJsonWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical includeSuggestions:(NSString*) includeSuggestions 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(includeSuggestions != nil)
        [queryParams setValue:includeSuggestions forKey:@"includeSuggestions"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getDefinitionsAsJsonWithCompletionBlock :(NSString*) word partOfSpeech:(NSString*) partOfSpeech sourceDictionaries:(NSString*) sourceDictionaries limit:(NSNumber*) limit includeRelated:(NSString*) includeRelated useCanonical:(NSString*) useCanonical includeTags:(NSString*) includeTags 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/definitions", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    if(partOfSpeech != nil)
        [queryParams setValue:partOfSpeech forKey:@"partOfSpeech"];
    if(includeRelated != nil)
        [queryParams setValue:includeRelated forKey:@"includeRelated"];
    if(sourceDictionaries != nil)
        [queryParams setValue:sourceDictionaries forKey:@"sourceDictionaries"];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(includeTags != nil)
        [queryParams setValue:includeTags forKey:@"includeTags"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getTopExampleAsJsonWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/topExample", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getRelatedWordsAsJsonWithCompletionBlock :(NSString*) word relationshipTypes:(NSString*) relationshipTypes useCanonical:(NSString*) useCanonical limitPerRelationshipType:(NSNumber*) limitPerRelationshipType 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/relatedWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(relationshipTypes != nil)
        [queryParams setValue:relationshipTypes forKey:@"relationshipTypes"];
    if(limitPerRelationshipType != nil)
        [queryParams setValue:limitPerRelationshipType forKey:@"limitPerRelationshipType"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getTextPronunciationsAsJsonWithCompletionBlock :(NSString*) word sourceDictionary:(NSString*) sourceDictionary typeFormat:(NSString*) typeFormat useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/pronunciations", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(sourceDictionary != nil)
        [queryParams setValue:sourceDictionary forKey:@"sourceDictionary"];
    if(typeFormat != nil)
        [queryParams setValue:typeFormat forKey:@"typeFormat"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getHyphenationAsJsonWithCompletionBlock :(NSString*) word sourceDictionary:(NSString*) sourceDictionary useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/hyphenation", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(sourceDictionary != nil)
        [queryParams setValue:sourceDictionary forKey:@"sourceDictionary"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getWordFrequencyAsJsonWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical startYear:(NSNumber*) startYear endYear:(NSNumber*) endYear 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/frequency", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(startYear != nil)
        [queryParams setValue:startYear forKey:@"startYear"];
    if(endYear != nil)
        [queryParams setValue:endYear forKey:@"endYear"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getPhrasesAsJsonWithCompletionBlock :(NSString*) word limit:(NSNumber*) limit wlmi:(NSNumber*) wlmi useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/phrases", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    if(wlmi != nil)
        [queryParams setValue:wlmi forKey:@"wlmi"];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getEtymologiesAsJsonWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/etymologies", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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

-(void) getAudioAsJsonWithCompletionBlock :(NSString*) word useCanonical:(NSString*) useCanonical limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/word.{format}/{word}/audio", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"word", @"}"]] withString: [_api escapeString:word]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(useCanonical != nil)
        [queryParams setValue:useCanonical forKey:@"useCanonical"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(word == nil) {
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


@end
