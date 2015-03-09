#import "SWGWordListsApi.h"
#import "SWGFile.h"
#import "SWGApiClient.h"
#import "SWGWordList.h"



@implementation SWGWordListsApi
static NSString * basePath = @"https://api.wordnik.com/v4";

+(SWGWordListsApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key {
    static SWGWordListsApi* singletonAPI = nil;

    if (singletonAPI == nil) {
        singletonAPI = [[SWGWordListsApi alloc] init];
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


-(NSNumber*) createWordListWithCompletionBlock:(SWGWordList*) body
         auth_token:(NSString*) auth_token
        
        completionHandler: (void (^)(SWGWordList* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/wordLists.json", basePath];

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
                
                SWGWordList *result = nil;
                if (data) {
                    result = [[SWGWordList    alloc]initWithValues: data];
                }
                completionBlock(result , nil);
                
              }];
    
    
}



@end