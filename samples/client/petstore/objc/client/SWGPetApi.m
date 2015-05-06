#import "SWGPetApi.h"
#import "SWGFile.h"
#import "SWGQueryParamCollection.h"
#import "SWGApiClient.h"
#import "SWGPet.h"
#import "SWGFile.h"


@interface SWGPetApi ()
    @property (readwrite, nonatomic, strong) NSMutableDictionary *defaultHeaders;
@end

@implementation SWGPetApi
static NSString * basePath = @"http://petstore.swagger.io/v2";

+(SWGPetApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key {
    static SWGPetApi* singletonAPI = nil;

    if (singletonAPI == nil) {
        singletonAPI = [[SWGPetApi alloc] init];
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
    [self.defaultHeaders setValue:value forKey:key];
}

-(id) init {
    self = [super init];
    self.defaultHeaders = [NSMutableDictionary dictionary];
    [self apiClient];
    return self;
}

-(void) setHeaderValue:(NSString*) value
           forKey:(NSString*)key {
    [self.defaultHeaders setValue:value forKey:key];
}

-(unsigned long) requestQueueSize {
    return [SWGApiClient requestQueueSize];
}


/*!
 * Update an existing pet
 * 
 * \param body Pet object that needs to be added to the store
 * \returns void
 */
-(NSNumber*) updatePetWithCompletionBlock: (SWGPet*) body
        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSArray* requestContentTypes = @[@"application/json", @"application/xml", ];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    id bodyDictionary = nil;
    
    id __body = body;

    if(__body != nil && [__body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)__body) {
            if([dict respondsToSelector:@selector(toDictionary)]) {
                [objs addObject:[(SWGObject*)dict toDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([__body respondsToSelector:@selector(toDictionary)]) {
        bodyDictionary = [(SWGObject*)__body toDictionary];
    }
    else if([__body isKindOfClass:[NSString class]]) {
        // convert it to a dictionary
        NSError * error;
        NSString * str = (NSString*)__body;
        NSDictionary *JSON =
            [NSJSONSerialization JSONObjectWithData: [str dataUsingEncoding: NSUTF8StringEncoding]
                                            options: NSJSONReadingMutableContainers
                                              error: &error];
        bodyDictionary = JSON;
    }
    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    

    
    // it's void
        return [client stringWithCompletionBlock: requestUrl 
                                      method: @"PUT" 
                                 queryParams: queryParams 
                                        body: bodyDictionary 
                                headerParams: headerParams
                          requestContentType: requestContentType
                         responseContentType: responseContentType
                             completionBlock: ^(NSString *data, NSError *error) {
                if (error) {
                    completionBlock(error);
                    return;
                }
                completionBlock(nil);
                    }];

    
}

/*!
 * Add a new pet to the store
 * 
 * \param body Pet object that needs to be added to the store
 * \returns void
 */
-(NSNumber*) addPetWithCompletionBlock: (SWGPet*) body
        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSArray* requestContentTypes = @[@"application/json", @"application/xml", ];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    id bodyDictionary = nil;
    
    id __body = body;

    if(__body != nil && [__body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)__body) {
            if([dict respondsToSelector:@selector(toDictionary)]) {
                [objs addObject:[(SWGObject*)dict toDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([__body respondsToSelector:@selector(toDictionary)]) {
        bodyDictionary = [(SWGObject*)__body toDictionary];
    }
    else if([__body isKindOfClass:[NSString class]]) {
        // convert it to a dictionary
        NSError * error;
        NSString * str = (NSString*)__body;
        NSDictionary *JSON =
            [NSJSONSerialization JSONObjectWithData: [str dataUsingEncoding: NSUTF8StringEncoding]
                                            options: NSJSONReadingMutableContainers
                                              error: &error];
        bodyDictionary = JSON;
    }
    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    

    
    // it's void
        return [client stringWithCompletionBlock: requestUrl 
                                      method: @"POST" 
                                 queryParams: queryParams 
                                        body: bodyDictionary 
                                headerParams: headerParams
                          requestContentType: requestContentType
                         responseContentType: responseContentType
                             completionBlock: ^(NSString *data, NSError *error) {
                if (error) {
                    completionBlock(error);
                    return;
                }
                completionBlock(nil);
                    }];

    
}

/*!
 * Finds Pets by status
 * Multiple status values can be provided with comma seperated strings
 * \param status Status values that need to be considered for filter
 * \returns NSArray<SWGPet>*
 */
-(NSNumber*) findPetsByStatusWithCompletionBlock: (NSArray*) status
        
        completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/findByStatus", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSArray* requestContentTypes = @[];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(status != nil) {
        
        queryParams[@"status"] = [[SWGQueryParamCollection alloc] initWithValuesAndFormat: status format: @"multi"];
        
        
    }
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    id bodyDictionary = nil;
    
    

    NSMutableDictionary * formParams = [[NSMutableDictionary alloc]init];

    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    // response is in a container
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
                        
                        
                        SWGPet* d = [[SWGPet alloc] initWithDictionary:dict error:nil];
                        
                        [objs addObject:d];
                    }
                    completionBlock((NSArray<SWGPet>*)objs, nil);
                }
                
                
            }];
    

    

    
}

/*!
 * Finds Pets by tags
 * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
 * \param tags Tags to filter by
 * \returns NSArray<SWGPet>*
 */
-(NSNumber*) findPetsByTagsWithCompletionBlock: (NSArray*) tags
        
        completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/findByTags", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSArray* requestContentTypes = @[];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(tags != nil) {
        
        queryParams[@"tags"] = [[SWGQueryParamCollection alloc] initWithValuesAndFormat: tags format: @"multi"];
        
        
    }
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    id bodyDictionary = nil;
    
    

    NSMutableDictionary * formParams = [[NSMutableDictionary alloc]init];

    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    
    // response is in a container
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
                        
                        
                        SWGPet* d = [[SWGPet alloc] initWithDictionary:dict error:nil];
                        
                        [objs addObject:d];
                    }
                    completionBlock((NSArray<SWGPet>*)objs, nil);
                }
                
                
            }];
    

    

    
}

/*!
 * Find pet by ID
 * Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions
 * \param petId ID of pet that needs to be fetched
 * \returns SWGPet*
 */
-(NSNumber*) getPetByIdWithCompletionBlock: (NSNumber*) petId
        
        completionHandler: (void (^)(SWGPet* output, NSError* error))completionBlock
         {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSArray* requestContentTypes = @[];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    id bodyDictionary = nil;
    
    

    NSMutableDictionary * formParams = [[NSMutableDictionary alloc]init];

    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    
    // non container response

    

    
    // complex response
        
    // comples response type
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
                SWGPet* result = nil;
                if (data) {
                    result = [[SWGPet  alloc]  initWithDictionary:data error:nil];
                }
                completionBlock(result , nil);
                
              }];
    

    

    
}

/*!
 * Updates a pet in the store with form data
 * 
 * \param petId ID of pet that needs to be updated
 * \param name Updated name of the pet
 * \param status Updated status of the pet
 * \returns void
 */
-(NSNumber*) updatePetWithFormWithCompletionBlock: (NSString*) petId
         name: (NSString*) name
         status: (NSString*) status
        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSArray* requestContentTypes = @[@"application/x-www-form-urlencoded", ];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    id bodyDictionary = nil;
    
    

    NSMutableDictionary * formParams = [[NSMutableDictionary alloc]init];

    
    
    formParams[@"name"] = name;
    
    if(bodyDictionary == nil) {
      bodyDictionary = [[NSMutableArray alloc] init];
    }
    [bodyDictionary addObject:formParams];
    
    
    formParams[@"status"] = status;
    
    if(bodyDictionary == nil) {
      bodyDictionary = [[NSMutableArray alloc] init];
    }
    [bodyDictionary addObject:formParams];
    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    

    
    // it's void
        return [client stringWithCompletionBlock: requestUrl 
                                      method: @"POST" 
                                 queryParams: queryParams 
                                        body: bodyDictionary 
                                headerParams: headerParams
                          requestContentType: requestContentType
                         responseContentType: responseContentType
                             completionBlock: ^(NSString *data, NSError *error) {
                if (error) {
                    completionBlock(error);
                    return;
                }
                completionBlock(nil);
                    }];

    
}

/*!
 * Deletes a pet
 * 
 * \param api_key 
 * \param petId Pet id to delete
 * \returns void
 */
-(NSNumber*) deletePetWithCompletionBlock: (NSString*) api_key
         petId: (NSNumber*) petId
        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSArray* requestContentTypes = @[];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    if(api_key != nil)
        headerParams[@"api_key"] = api_key;
    

    id bodyDictionary = nil;
    
    

    NSMutableDictionary * formParams = [[NSMutableDictionary alloc]init];

    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    

    
    // it's void
        return [client stringWithCompletionBlock: requestUrl 
                                      method: @"DELETE" 
                                 queryParams: queryParams 
                                        body: bodyDictionary 
                                headerParams: headerParams
                          requestContentType: requestContentType
                         responseContentType: responseContentType
                             completionBlock: ^(NSString *data, NSError *error) {
                if (error) {
                    completionBlock(error);
                    return;
                }
                completionBlock(nil);
                    }];

    
}

/*!
 * uploads an image
 * 
 * \param petId ID of pet to update
 * \param additionalMetadata Additional data to pass to server
 * \param file file to upload
 * \returns void
 */
-(NSNumber*) uploadFileWithCompletionBlock: (NSNumber*) petId
         additionalMetadata: (NSString*) additionalMetadata
         file: (SWGFile*) file
        
        
        completionHandler: (void (^)(NSError* error))completionBlock {

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}/uploadImage", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSArray* requestContentTypes = @[@"multipart/form-data", ];
    NSString* requestContentType = [requestContentTypes count] > 0 ? requestContentTypes[0] : @"application/json";

    NSArray* responseContentTypes = @[@"application/json", @"application/xml"];
    NSString* responseContentType = [responseContentTypes count] > 0 ? responseContentTypes[0] : @"application/json";

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    id bodyDictionary = nil;
    
    

    NSMutableDictionary * formParams = [[NSMutableDictionary alloc]init];

    
    
    formParams[@"additionalMetadata"] = additionalMetadata;
    
    if(bodyDictionary == nil) {
      bodyDictionary = [[NSMutableArray alloc] init];
    }
    [bodyDictionary addObject:formParams];
    
    
    requestContentType = @"multipart/form-data";
    if(bodyDictionary == nil) {
      bodyDictionary = [[NSMutableArray alloc] init];
    }
    if(file != nil) {
      [bodyDictionary addObject:file];
      file.paramName  = @"file";
    }
    
    if(bodyDictionary == nil) {
      bodyDictionary = [[NSMutableArray alloc] init];
    }
    [bodyDictionary addObject:formParams];
    
    

    

    SWGApiClient* client = [SWGApiClient sharedClientFromPool:basePath];

    

    

    
    // it's void
        return [client stringWithCompletionBlock: requestUrl 
                                      method: @"POST" 
                                 queryParams: queryParams 
                                        body: bodyDictionary 
                                headerParams: headerParams
                          requestContentType: requestContentType
                         responseContentType: responseContentType
                             completionBlock: ^(NSString *data, NSError *error) {
                if (error) {
                    completionBlock(error);
                    return;
                }
                completionBlock(nil);
                    }];

    
}



@end
