#import "SWGAccountApi.h"
#import "SWGFile.h"
#import "SWGApiClient.h"
#import "SWGApiTokenStatus.h"
#import "SWGAuthenticationToken.h"
#import "SWGUser.h"
#import "SWGWordList.h"



@implementation SWGAccountApi
static NSString * basePath = @"https://api.wordnik.com/v4";

+(SWGAccountApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key {
    static SWGAccountApi* singletonAPI = nil;

    if (singletonAPI == nil) {
        singletonAPI = [[SWGAccountApi alloc] init];
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


-(NSNumber*) getApiTokenStatusWithCompletionBlock:(NSString*) api_key
        
        completionHandler: (void (^)(SWGApiTokenStatus* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/account.json/apiTokenStatus", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(api_key != nil)
        headerParams[@"api_key"] = api_key;
    

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
                
                SWGApiTokenStatus *result = nil;
                if (data) {
                    result = [[SWGApiTokenStatus    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}

-(NSNumber*) authenticateWithCompletionBlock:(NSString*) username
         password:(NSString*) password
        
        completionHandler: (void (^)(SWGAuthenticationToken* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/account.json/authenticate/{username}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString: [SWGApiClient escape:username]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(password != nil)
        queryParams[@"password"] = password;
    
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
                
                SWGAuthenticationToken *result = nil;
                if (data) {
                    result = [[SWGAuthenticationToken    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}

-(NSNumber*) authenticatePostWithCompletionBlock:(NSString*) username
         body:(NSString*) body
        
        completionHandler: (void (^)(SWGAuthenticationToken* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/account.json/authenticate/{username}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString: [SWGApiClient escape:username]];
    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    

    id bodyDictionary = nil;
    
    id __body = body;

    if(__body != nil && [__body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)__body) {
            if([dict respondsToSelector:@selector(asDictionary)]) {
                [objs addObject:[(SWGObject*)dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([__body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [(SWGObject*)__body asDictionary];
    }
    else if([__body isKindOfClass:[NSString class]]) {
        // convert it to a dictionary
        NSError * error;
        NSString * str = (NSString*)__body;
        NSDictionary *JSON =
            [NSJSONSerialization JSONObjectWithData:[str dataUsingEncoding:NSUTF8StringEncoding]
                                            options:NSJSONReadingMutableContainers
                                              error:&error];
        bodyDictionary = JSON;
    }
    else if([__body isKindOfClass: [SWGFile class]]) {
        requestContentType = @"form-data";
        bodyDictionary = __body;
    }
    else{
        NSLog(@"don't know what to do with %@", __body);
    }
    
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    
    
        
    // comples response type
    return [client dictionary:requestUrl 
                       method:@"POST" 
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
                
                SWGAuthenticationToken *result = nil;
                if (data) {
                    result = [[SWGAuthenticationToken    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}

-(NSNumber*) getLoggedInUserWithCompletionBlock:(NSString*) auth_token
        
        completionHandler: (void (^)(SWGUser* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/account.json/user", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        headerParams[@"auth_token"] = auth_token;
    

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
                
                SWGUser *result = nil;
                if (data) {
                    result = [[SWGUser    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}

-(NSNumber*) getWordListsForLoggedInUserWithCompletionBlock:(NSString*) auth_token
         skip:(NSNumber*) skip
         limit:(NSNumber*) limit
        
        completionHandler: (void (^)(NSArray* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/account.json/wordLists", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSString* requestContentType = @"application/json";
    NSString* responseContentType = @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(skip != nil)
        queryParams[@"skip"] = skip;
    if(limit != nil)
        queryParams[@"limit"] = limit;
    
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    if(auth_token != nil)
        headerParams[@"auth_token"] = auth_token;
    

    id bodyDictionary = nil;
    
    
    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

        // array container response type
    return [client dictionary: requestUrl 
                       method: @"GET" 
                  queryParams: queryParams 
                         body: bodyDictionary 
                 headerParams: headerParams
           requestContentType: requestContentType
          responseContentType: responseContentType
              completionBlock: ^(NSDictionary *data, NSError *error) {
                 if (error) {
                     completionBlock(nil, error);
                     
                     return;
                 }
                 
                 if([data isKindOfClass:[NSArray class]]){
                     NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
                     for (NSDictionary* dict in (NSArray*)data) {
                        
                        
                        SWGWordList* d = [[SWGWordList alloc]initWithValues: dict];
                        
                        [objs addObject:d];
                     }
                     completionBlock(objs, nil);
                 }
                

                
            }];
    
    
}



@end