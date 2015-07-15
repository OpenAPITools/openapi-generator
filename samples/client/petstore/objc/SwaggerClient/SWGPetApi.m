#import "SWGPetApi.h"
#import "SWGQueryParamCollection.h"
#import "SWGPet.h"


@interface SWGPetApi ()
    @property (readwrite, nonatomic, strong) NSMutableDictionary *defaultHeaders;
@end

@implementation SWGPetApi

static NSString * basePath = @"http://petstore.swagger.io/v2";

#pragma mark - Initialize methods

- (id) init {
    self = [super init];
    if (self) {
        self.apiClient = [SWGApiClient sharedClientFromPool:basePath];
        self.defaultHeaders = [NSMutableDictionary dictionary];
    }
    return self;
}

- (id) initWithApiClient:(SWGApiClient *)apiClient {
    self = [super init];
    if (self) {
        if (apiClient) {
            self.apiClient = apiClient;
        }
        else {
            self.apiClient = [SWGApiClient sharedClientFromPool:basePath];
        }
        self.defaultHeaders = [NSMutableDictionary dictionary];
    }
    return self;
}

#pragma mark -

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

-(void) addHeader:(NSString*)value forKey:(NSString*)key {
    [self.defaultHeaders setValue:value forKey:key];
}

-(void) setHeaderValue:(NSString*) value
           forKey:(NSString*)key {
    [self.defaultHeaders setValue:value forKey:key];
}

-(unsigned long) requestQueueSize {
    return [SWGApiClient requestQueueSize];
}

#pragma mark - Api Methods

///
/// Update an existing pet
/// 
///  @param body Pet object that needs to be added to the store
///
///  @returns void
///
-(NSNumber*) updatePetWithCompletionBlock: (SWGPet*) body
        
        
        completionHandler: (void (^)(NSError* error))completionBlock { 

    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[@"application/json", @"application/xml"]];

    // Authentication setting
    NSArray *authSettings = @[@"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    bodyParam = body;

    if(bodyParam != nil && [bodyParam isKindOfClass:[NSArray class]]){
        NSMutableArray *objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)bodyParam) {
            if([dict respondsToSelector:@selector(toDictionary)]) {
                [objs addObject:[(SWGObject*)dict toDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyParam = objs;
    }
    else if([bodyParam respondsToSelector:@selector(toDictionary)]) {
        bodyParam = [(SWGObject*)bodyParam toDictionary];
    }
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"PUT"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: nil
                                      completionBlock: ^(id data, NSError *error) {
                  completionBlock(error);
                  
              }
          ];
}

///
/// Add a new pet to the store
/// 
///  @param body Pet object that needs to be added to the store
///
///  @returns void
///
-(NSNumber*) addPetWithCompletionBlock: (SWGPet*) body
        
        
        completionHandler: (void (^)(NSError* error))completionBlock { 

    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[@"application/json", @"application/xml"]];

    // Authentication setting
    NSArray *authSettings = @[@"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    bodyParam = body;

    if(bodyParam != nil && [bodyParam isKindOfClass:[NSArray class]]){
        NSMutableArray *objs = [[NSMutableArray alloc] init];
        for (id dict in (NSArray*)bodyParam) {
            if([dict respondsToSelector:@selector(toDictionary)]) {
                [objs addObject:[(SWGObject*)dict toDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyParam = objs;
    }
    else if([bodyParam respondsToSelector:@selector(toDictionary)]) {
        bodyParam = [(SWGObject*)bodyParam toDictionary];
    }
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"POST"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: nil
                                      completionBlock: ^(id data, NSError *error) {
                  completionBlock(error);
                  
              }
          ];
}

///
/// Finds Pets by status
/// Multiple status values can be provided with comma seperated strings
///  @param status Status values that need to be considered for filter
///
///  @returns NSArray<SWGPet>*
///
-(NSNumber*) findPetsByStatusWithCompletionBlock: (NSArray*) status
        
        completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock { 
        

    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/findByStatus", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(status != nil) {
        
        queryParams[@"status"] = [[SWGQueryParamCollection alloc] initWithValuesAndFormat: status format: @"multi"];
        
        
    }
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[]];

    // Authentication setting
    NSArray *authSettings = @[@"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"GET"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: @"NSArray<SWGPet>*"
                                      completionBlock: ^(id data, NSError *error) {
                  
                  completionBlock((NSArray<SWGPet>*)data, error);
              }
          ];
}

///
/// Finds Pets by tags
/// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
///  @param tags Tags to filter by
///
///  @returns NSArray<SWGPet>*
///
-(NSNumber*) findPetsByTagsWithCompletionBlock: (NSArray*) tags
        
        completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error))completionBlock { 
        

    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/findByTags", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(tags != nil) {
        
        queryParams[@"tags"] = [[SWGQueryParamCollection alloc] initWithValuesAndFormat: tags format: @"multi"];
        
        
    }
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[]];

    // Authentication setting
    NSArray *authSettings = @[@"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"GET"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: @"NSArray<SWGPet>*"
                                      completionBlock: ^(id data, NSError *error) {
                  
                  completionBlock((NSArray<SWGPet>*)data, error);
              }
          ];
}

///
/// Find pet by ID
/// Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions
///  @param petId ID of pet that needs to be fetched
///
///  @returns SWGPet*
///
-(NSNumber*) getPetByIdWithCompletionBlock: (NSNumber*) petId
        
        completionHandler: (void (^)(SWGPet* output, NSError* error))completionBlock { 
        

    
    // verify the required parameter 'petId' is set
    if (petId == nil) {
        [NSException raise:@"Invalid parameter" format:@"Missing the required parameter `petId` when calling `getPetById`"];
    }
    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[]];

    // Authentication setting
    NSArray *authSettings = @[@"api_key", @"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"GET"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: @"SWGPet*"
                                      completionBlock: ^(id data, NSError *error) {
                  
                  completionBlock((SWGPet*)data, error);
              }
          ];
}

///
/// Updates a pet in the store with form data
/// 
///  @param petId ID of pet that needs to be updated
///
///  @param name Updated name of the pet
///
///  @param status Updated status of the pet
///
///  @returns void
///
-(NSNumber*) updatePetWithFormWithCompletionBlock: (NSString*) petId
         name: (NSString*) name
         status: (NSString*) status
        
        
        completionHandler: (void (^)(NSError* error))completionBlock { 

    
    // verify the required parameter 'petId' is set
    if (petId == nil) {
        [NSException raise:@"Invalid parameter" format:@"Missing the required parameter `petId` when calling `updatePetWithForm`"];
    }
    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[@"application/x-www-form-urlencoded"]];

    // Authentication setting
    NSArray *authSettings = @[@"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    
    
    formParams[@"name"] = name;
    
    
    
    formParams[@"status"] = status;
    
    
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"POST"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: nil
                                      completionBlock: ^(id data, NSError *error) {
                  completionBlock(error);
                  
              }
          ];
}

///
/// Deletes a pet
/// 
///  @param apiKey 
///
///  @param petId Pet id to delete
///
///  @returns void
///
-(NSNumber*) deletePetWithCompletionBlock: (NSString*) apiKey
         petId: (NSNumber*) petId
        
        
        completionHandler: (void (^)(NSError* error))completionBlock { 

    
    // verify the required parameter 'petId' is set
    if (petId == nil) {
        [NSException raise:@"Invalid parameter" format:@"Missing the required parameter `petId` when calling `deletePet`"];
    }
    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    if(apiKey != nil)
        headerParams[@"api_key"] = apiKey;
    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[]];

    // Authentication setting
    NSArray *authSettings = @[@"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"DELETE"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: nil
                                      completionBlock: ^(id data, NSError *error) {
                  completionBlock(error);
                  
              }
          ];
}

///
/// uploads an image
/// 
///  @param petId ID of pet to update
///
///  @param additionalMetadata Additional data to pass to server
///
///  @param file file to upload
///
///  @returns void
///
-(NSNumber*) uploadFileWithCompletionBlock: (NSNumber*) petId
         additionalMetadata: (NSString*) additionalMetadata
         file: (NSURL*) file
        
        
        completionHandler: (void (^)(NSError* error))completionBlock { 

    
    // verify the required parameter 'petId' is set
    if (petId == nil) {
        [NSException raise:@"Invalid parameter" format:@"Missing the required parameter `petId` when calling `uploadFile`"];
    }
    

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet/{petId}/uploadImage", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [SWGApiClient escape:petId]];
    

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.defaultHeaders];

    

    // HTTP header `Accept`
    headerParams[@"Accept"] = [SWGApiClient selectHeaderAccept:@[@"application/json", @"application/xml"]];
    if ([headerParams[@"Accept"] length] == 0) {
        [headerParams removeObjectForKey:@"Accept"];
    }

    // response content type
    NSString *responseContentType;
    if ([headerParams objectForKey:@"Accept"]) {
        responseContentType = [headerParams[@"Accept"] componentsSeparatedByString:@", "][0];
    }
    else {
        responseContentType = @"";
    }

    // request content type
    NSString *requestContentType = [SWGApiClient selectHeaderContentType:@[@"multipart/form-data"]];

    // Authentication setting
    NSArray *authSettings = @[@"petstore_auth"];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *files = [[NSMutableDictionary alloc] init];
    
    
    
    formParams[@"additionalMetadata"] = additionalMetadata;
    
    
    
    files[@"file"] = file;
    
    
    

    
    return [self.apiClient requestWithCompletionBlock: requestUrl
                                               method: @"POST"
                                          queryParams: queryParams
                                           formParams: formParams
                                                files: files
                                                 body: bodyParam
                                         headerParams: headerParams
                                         authSettings: authSettings
                                   requestContentType: requestContentType
                                  responseContentType: responseContentType
                                         responseType: nil
                                      completionBlock: ^(id data, NSError *error) {
                  completionBlock(error);
                  
              }
          ];
}



@end
