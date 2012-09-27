#import "NIKWordListApi.h"
#import "NIKWordList.h"
#import "NIKStringValue.h"
#import "NIKWordListWord.h"



@implementation NIKWordListApi
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
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) updateWordListWithCompletionBlock :(NSString*) permalink body:(NIKWordList*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
        if(body != nil && [body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)body) {
            if([dict respondsToSelector:@selector(asDictionary)]) {
                [objs addObject:[(NIKSwaggerObject*)dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [(NIKSwaggerObject*)body asDictionary];
    }
    else if([body isKindOfClass:[NSString class]]) {
        bodyDictionary = body;
    }
    else{
        NSLog(@"don't know what to do with %@", body);
    }

    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api stringWithCompletionBlock: requestUrl 
                             method: @"PUT" 
                        queryParams: queryParams 
                               body: bodyDictionary 
                       headerParams: headerParams
                  completionHandler: ^(NSString *data, NSError *error) {
        if (error) {
            completionBlock(error);
            return;
        }
        completionBlock(nil);
    }];
    
}

/**
 * 
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) deleteWordListWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
        if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api stringWithCompletionBlock: requestUrl 
                             method: @"DELETE" 
                        queryParams: queryParams 
                               body: bodyDictionary 
                       headerParams: headerParams
                  completionHandler: ^(NSString *data, NSError *error) {
        if (error) {
            completionBlock(error);
            return;
        }
        completionBlock(nil);
    }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKWordList
 * returnContainer: 
 * 
 **/
-(void) getWordListByPermalinkWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NIKWordList*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
        if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
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
        
        completionBlock( [[NIKWordList alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) addWordsToWordListWithCompletionBlock :(NSString*) permalink body:(NSArray*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}/words", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
        if(body != nil && [body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)body) {
            if([dict respondsToSelector:@selector(asDictionary)]) {
                [objs addObject:[(NIKSwaggerObject*)dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [(NIKSwaggerObject*)body asDictionary];
    }
    else if([body isKindOfClass:[NSString class]]) {
        bodyDictionary = body;
    }
    else{
        NSLog(@"don't know what to do with %@", body);
    }

    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api stringWithCompletionBlock: requestUrl 
                             method: @"POST" 
                        queryParams: queryParams 
                               body: bodyDictionary 
                       headerParams: headerParams
                  completionHandler: ^(NSString *data, NSError *error) {
        if (error) {
            completionBlock(error);
            return;
        }
        completionBlock(nil);
    }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKWordListWord
 * returnContainer: List
 * 
 **/
-(void) getWordListWordsWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}/words", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(sortBy != nil)
        [queryParams setValue:sortBy forKey:@"sortBy"];
    if(sortOrder != nil)
        [queryParams setValue:sortOrder forKey:@"sortOrder"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
        if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
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
                NIKWordListWord* d = [[NIKWordListWord alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) deleteWordsFromWordListWithCompletionBlock :(NSString*) permalink body:(NSArray*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}/deleteWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
        if(body != nil && [body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)body) {
            if([dict respondsToSelector:@selector(asDictionary)]) {
                [objs addObject:[(NIKSwaggerObject*)dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [(NIKSwaggerObject*)body asDictionary];
    }
    else if([body isKindOfClass:[NSString class]]) {
        bodyDictionary = body;
    }
    else{
        NSLog(@"don't know what to do with %@", body);
    }

    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api stringWithCompletionBlock: requestUrl 
                             method: @"POST" 
                        queryParams: queryParams 
                               body: bodyDictionary 
                       headerParams: headerParams
                  completionHandler: ^(NSString *data, NSError *error) {
        if (error) {
            completionBlock(error);
            return;
        }
        completionBlock(nil);
    }];
    
}

-(void) updateWordListAsJsonWithCompletionBlock :(NSString*) permalink body:(NIKWordList*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
    if(body != nil && [body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)body) {
            if([dict respondsToSelector:@selector(asDictionary)]) {
                [objs addObject:[(NIKSwaggerObject*)dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [(NIKSwaggerObject*)body asDictionary];
    }
    else if([body isKindOfClass:[NSString class]]) {
        bodyDictionary = body;
    }
    else{
        NSLog(@"don't know what to do with %@", body);
    }

    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"PUT" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }

        completionBlock(nil);
    }];

}

-(void) deleteWordListAsJsonWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"DELETE" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }

        completionBlock(nil);
    }];

}

-(void) getWordListByPermalinkAsJsonWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
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

-(void) addWordsToWordListAsJsonWithCompletionBlock :(NSString*) permalink body:(NSArray*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}/words", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
    if(body != nil && [body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)body) {
            if([dict respondsToSelector:@selector(asDictionary)]) {
                [objs addObject:[(NIKSwaggerObject*)dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [(NIKSwaggerObject*)body asDictionary];
    }
    else if([body isKindOfClass:[NSString class]]) {
        bodyDictionary = body;
    }
    else{
        NSLog(@"don't know what to do with %@", body);
    }

    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"POST" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }

        completionBlock(nil);
    }];

}

-(void) getWordListWordsAsJsonWithCompletionBlock :(NSString*) permalink auth_token:(NSString*) auth_token sortBy:(NSString*) sortBy sortOrder:(NSString*) sortOrder skip:(NSNumber*) skip limit:(NSNumber*) limit 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}/words", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(sortBy != nil)
        [queryParams setValue:sortBy forKey:@"sortBy"];
    if(sortOrder != nil)
        [queryParams setValue:sortOrder forKey:@"sortOrder"];
    if(skip != nil)
        [queryParams setValue:skip forKey:@"skip"];
    if(limit != nil)
        [queryParams setValue:limit forKey:@"limit"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
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

-(void) deleteWordsFromWordListAsJsonWithCompletionBlock :(NSString*) permalink body:(NSArray*) body auth_token:(NSString*) auth_token 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordList.{format}/{permalink}/deleteWords", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"permalink", @"}"]] withString: [_api escapeString:permalink]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        [headerParams setValue:auth_token forKey:@"auth_token"];
    id bodyDictionary = nil;
    if(body != nil && [body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)body) {
            if([dict respondsToSelector:@selector(asDictionary)]) {
                [objs addObject:[(NIKSwaggerObject*)dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [(NIKSwaggerObject*)body asDictionary];
    }
    else if([body isKindOfClass:[NSString class]]) {
        bodyDictionary = body;
    }
    else{
        NSLog(@"don't know what to do with %@", body);
    }

    if(permalink == nil) {
        // error
    }
    if(auth_token == nil) {
        // error
    }
    [_api dictionaryWithCompletionBlock: requestUrl 
                                 method: @"POST" 
                            queryParams: queryParams 
                                   body: bodyDictionary 
                           headerParams: headerParams
                      completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }

        completionBlock(nil);
    }];

}


@end
