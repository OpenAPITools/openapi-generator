#import "NIKPetApi.h"
#import "NIKPet.h"



@implementation NIKPetApi
static NSString * basePath = @"http://petstore.swagger.wordnik.com/api";

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
 * returnBaseType: NIKPet
 * returnContainer: 
 * 
 **/
-(void) getPetByIdWithCompletionBlock :(NSString*) petId 
        completionHandler:(void (^)(NIKPet*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [_api escapeString:petId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(petId == nil) {
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
        
        completionBlock( [[NIKPet alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) addPetWithCompletionBlock :(NIKPet*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
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

    if(body == nil) {
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
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) updatePetWithCompletionBlock :(NIKPet*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
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

    if(body == nil) {
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
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKPet
 * returnContainer: List
 * 
 **/
-(void) findPetsByStatusWithCompletionBlock :(NSString*) status 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByStatus", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(status != nil)
        [queryParams setValue:status forKey:@"status"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(status == nil) {
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
                NIKPet* d = [[NIKPet alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

/**
 * 
 * returnTypeIsPrimitive: 
 * returnBaseType: NIKPet
 * returnContainer: List
 * 
 **/
-(void) findPetsByTagsWithCompletionBlock :(NSString*) tags 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByTags", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(tags != nil)
        [queryParams setValue:tags forKey:@"tags"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(tags == nil) {
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
                NIKPet* d = [[NIKPet alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        }];
    
}

-(void) getPetByIdAsJsonWithCompletionBlock :(NSString*) petId 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [_api escapeString:petId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(petId == nil) {
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

-(void) addPetAsJsonWithCompletionBlock :(NIKPet*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
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

    if(body == nil) {
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

-(void) updatePetAsJsonWithCompletionBlock :(NIKPet*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
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

    if(body == nil) {
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

-(void) findPetsByStatusAsJsonWithCompletionBlock :(NSString*) status 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByStatus", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(status != nil)
        [queryParams setValue:status forKey:@"status"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(status == nil) {
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

-(void) findPetsByTagsAsJsonWithCompletionBlock :(NSString*) tags 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByTags", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(tags != nil)
        [queryParams setValue:tags forKey:@"tags"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(tags == nil) {
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
