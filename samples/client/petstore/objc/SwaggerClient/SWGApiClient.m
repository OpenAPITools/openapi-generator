#import "SWGApiClient.h"

@implementation SWGApiClient

NSString *const SWGResponseObjectErrorKey = @"SWGResponseObject";

static long requestId = 0;
static bool offlineState = false;
static NSMutableSet * queuedRequests = nil;
static bool cacheEnabled = false;
static AFNetworkReachabilityStatus reachabilityStatus = AFNetworkReachabilityStatusNotReachable;
static NSOperationQueue* sharedQueue;
static void (^reachabilityChangeBlock)(int);
static bool loggingEnabled = true;

#pragma mark - Log Methods

+(void)setLoggingEnabled:(bool) state {
    loggingEnabled = state;
}

- (void)logRequest:(NSURLRequest*)request {
    NSLog(@"request: %@", [self descriptionForRequest:request]);
}

- (void)logResponse:(id)data forRequest:(NSURLRequest*)request error:(NSError*)error {
    NSLog(@"request: %@  response: %@ ",  [self descriptionForRequest:request], data );
}

#pragma mark -

+(void)clearCache {
    [[NSURLCache sharedURLCache] removeAllCachedResponses];
}

+(void)setCacheEnabled:(BOOL)enabled {
    cacheEnabled = enabled;
}

+(void)configureCacheWithMemoryAndDiskCapacity: (unsigned long) memorySize
                                      diskSize: (unsigned long) diskSize {
    NSAssert(memorySize > 0, @"invalid in-memory cache size");
    NSAssert(diskSize >= 0, @"invalid disk cache size");

    NSURLCache *cache =
    [[NSURLCache alloc]
     initWithMemoryCapacity:memorySize
     diskCapacity:diskSize
     diskPath:@"swagger_url_cache"];

    [NSURLCache setSharedURLCache:cache];
}

+(NSOperationQueue*) sharedQueue {
    return sharedQueue;
}

+(SWGApiClient *)sharedClientFromPool:(NSString *)baseUrl {
    static NSMutableDictionary *_pool = nil;
    if (queuedRequests == nil) {
        queuedRequests = [[NSMutableSet alloc]init];
    }
    if(_pool == nil) {
        // setup static vars
        // create queue
        sharedQueue = [[NSOperationQueue alloc] init];

        // create pool
        _pool = [[NSMutableDictionary alloc] init];

        // initialize URL cache
        [SWGApiClient configureCacheWithMemoryAndDiskCapacity:4*1024*1024 diskSize:32*1024*1024];

        // configure reachability
        [SWGApiClient configureCacheReachibilityForHost:baseUrl];
    }

    @synchronized(self) {
        SWGApiClient * client = [_pool objectForKey:baseUrl];
        if (client == nil) {
            client = [[SWGApiClient alloc] initWithBaseURL:[NSURL URLWithString:baseUrl]];
            [_pool setValue:client forKey:baseUrl ];
            if(loggingEnabled)
                NSLog(@"new client for path %@", baseUrl);
        }
        if(loggingEnabled)
            NSLog(@"returning client for path %@", baseUrl);
        return client;
    }
}

/*
 * Detect `Accept` from accepts
 */
+ (NSString *) selectHeaderAccept:(NSArray *)accepts
{
    if (accepts == nil || [accepts count] == 0) {
        return @"";
    }
    
    NSMutableArray *lowerAccepts = [[NSMutableArray alloc] initWithCapacity:[accepts count]];
    [accepts enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
        [lowerAccepts addObject:[obj lowercaseString]];
    }];

    
    if ([lowerAccepts containsObject:@"application/json"]) {
        return @"application/json";
    }
    else {
        return [lowerAccepts componentsJoinedByString:@", "];
    }
}

/*
 * Detect `Content-Type` from contentTypes
 */
+ (NSString *) selectHeaderContentType:(NSArray *)contentTypes
{
    if (contentTypes == nil || [contentTypes count] == 0) {
        return @"application/json";
    }
    
    NSMutableArray *lowerContentTypes = [[NSMutableArray alloc] initWithCapacity:[contentTypes count]];
    [contentTypes enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
        [lowerContentTypes addObject:[obj lowercaseString]];
    }];

    if ([lowerContentTypes containsObject:@"application/json"]) {
        return @"application/json";
    }
    else {
        return lowerContentTypes[0];
    }
}

-(void)setHeaderValue:(NSString*) value
               forKey:(NSString*) forKey {
    [self.requestSerializer setValue:value forHTTPHeaderField:forKey];
}

+(unsigned long)requestQueueSize {
    return [queuedRequests count];
}

+(NSNumber*) nextRequestId {
    @synchronized(self) {
        long nextId = ++requestId;
        if(loggingEnabled)
            NSLog(@"got id %ld", nextId);
        return [NSNumber numberWithLong:nextId];
    }
}

+(NSNumber*) queueRequest {
    NSNumber* requestId = [SWGApiClient nextRequestId];
    if(loggingEnabled)
        NSLog(@"added %@ to request queue", requestId);
    [queuedRequests addObject:requestId];
    return requestId;
}

+(void) cancelRequest:(NSNumber*)requestId {
    [queuedRequests removeObject:requestId];
}

+(NSString*) escape:(id)unescaped {
    if([unescaped isKindOfClass:[NSString class]]){
        return (NSString *)CFBridgingRelease
        (CFURLCreateStringByAddingPercentEscapes(
                                                 NULL,
                                                 (__bridge CFStringRef) unescaped,
                                                 NULL,
                                                 (CFStringRef)@"!*'();:@&=+$,/?%#[]",
                                                 kCFStringEncodingUTF8));
    }
    else {
        return [NSString stringWithFormat:@"%@", unescaped];
    }
}

-(Boolean) executeRequestWithId:(NSNumber*) requestId {
    NSSet* matchingItems = [queuedRequests objectsPassingTest:^BOOL(id obj, BOOL *stop) {
        if([obj intValue]  == [requestId intValue])
            return TRUE;
        else return FALSE;
    }];

    if(matchingItems.count == 1) {
        if(loggingEnabled)
            NSLog(@"removing request id %@", requestId);
        [queuedRequests removeObject:requestId];
        return true;
    }
    else
        return false;
}

-(id)initWithBaseURL:(NSURL *)url {
    self = [super initWithBaseURL:url];
    self.requestSerializer = [AFJSONRequestSerializer serializer];
    self.responseSerializer = [AFJSONResponseSerializer serializer];
    if (!self)
        return nil;
    return self;
}

+(AFNetworkReachabilityStatus) getReachabilityStatus {
    return reachabilityStatus;
}

+(void) setReachabilityChangeBlock:(void(^)(int))changeBlock {
    reachabilityChangeBlock = changeBlock;
}

+(void) setOfflineState:(BOOL) state {
    offlineState = state;
}

+(void) configureCacheReachibilityForHost:(NSString*)host {
    [[SWGApiClient sharedClientFromPool:host].reachabilityManager setReachabilityStatusChangeBlock:^(AFNetworkReachabilityStatus status) {
        reachabilityStatus = status;
        switch (status) {
            case AFNetworkReachabilityStatusUnknown:
                if(loggingEnabled)
                    NSLog(@"reachability changed to AFNetworkReachabilityStatusUnknown");
                [SWGApiClient setOfflineState:true];
                break;

            case AFNetworkReachabilityStatusNotReachable:
                if(loggingEnabled)
                    NSLog(@"reachability changed to AFNetworkReachabilityStatusNotReachable");
                [SWGApiClient setOfflineState:true];
                break;

            case AFNetworkReachabilityStatusReachableViaWWAN:
                if(loggingEnabled)
                    NSLog(@"reachability changed to AFNetworkReachabilityStatusReachableViaWWAN");
                [SWGApiClient setOfflineState:false];
                break;

            case AFNetworkReachabilityStatusReachableViaWiFi:
                if(loggingEnabled)
                    NSLog(@"reachability changed to AFNetworkReachabilityStatusReachableViaWiFi");
                [SWGApiClient setOfflineState:false];
                break;
            default:
                break;
        }
        // call the reachability block, if configured
        if(reachabilityChangeBlock != nil) {
            reachabilityChangeBlock(status);
        }
    }];
    [[SWGApiClient sharedClientFromPool:host].reachabilityManager startMonitoring];
}

-(NSString*) pathWithQueryParamsToString:(NSString*) path
                             queryParams:(NSDictionary*) queryParams {
    NSString * separator = nil;
    int counter = 0;

    NSMutableString * requestUrl = [NSMutableString stringWithFormat:@"%@", path];
    if(queryParams != nil){
        for(NSString * key in [queryParams keyEnumerator]){
            if(counter == 0) separator = @"?";
            else separator = @"&";
            NSString * value;
            id queryParam = [queryParams valueForKey:key];
            if([queryParam isKindOfClass:[NSString class]]){
                [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                                          [SWGApiClient escape:key], [SWGApiClient escape:[queryParams valueForKey:key]]]];
            }
            else if([queryParam isKindOfClass:[SWGQueryParamCollection class]]){
                SWGQueryParamCollection * coll = (SWGQueryParamCollection*) queryParam;
                NSArray* values = [coll values];
                NSString* format = [coll format];

                if([format isEqualToString:@"csv"]) {
                    [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                        [SWGApiClient escape:key], [NSString stringWithFormat:@"%@", [values componentsJoinedByString:@","]]]];

                }
                else if([format isEqualToString:@"tsv"]) {
                    [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                        [SWGApiClient escape:key], [NSString stringWithFormat:@"%@", [values componentsJoinedByString:@"\t"]]]];

                }
                else if([format isEqualToString:@"pipes"]) {
                    [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                        [SWGApiClient escape:key], [NSString stringWithFormat:@"%@", [values componentsJoinedByString:@"|"]]]];

                }
                else if([format isEqualToString:@"multi"]) {
                    for(id obj in values) {
                        [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                            [SWGApiClient escape:key], [NSString stringWithFormat:@"%@", obj]]];
                        counter += 1;
                    }

                }
            }
            else {
                [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                                          [SWGApiClient escape:key], [NSString stringWithFormat:@"%@", [queryParams valueForKey:key]]]];
            }

            counter += 1;
        }
    }
    return requestUrl;
}

- (NSString*)descriptionForRequest:(NSURLRequest*)request {
    return [[request URL] absoluteString];
}


/**
 * Update header and query params based on authentication settings
 */
- (void) updateHeaderParams:(NSDictionary *__autoreleasing *)headers
                queryParams:(NSDictionary *__autoreleasing *)querys
           WithAuthSettings:(NSArray *)authSettings {
    
    if (!authSettings || [authSettings count] == 0) {
        return;
    }
    
    NSMutableDictionary *headersWithAuth = [NSMutableDictionary dictionaryWithDictionary:*headers];
    NSMutableDictionary *querysWithAuth = [NSMutableDictionary dictionaryWithDictionary:*querys];
    
    SWGConfiguration *config = [SWGConfiguration sharedConfig];
    for (NSString *auth in authSettings) {
        NSDictionary *authSetting = [[config authSettings] objectForKey:auth];
        
        if (authSetting) {
            if ([authSetting[@"in"] isEqualToString:@"header"]) {
                [headersWithAuth setObject:authSetting[@"value"] forKey:authSetting[@"key"]];
            }
            else if ([authSetting[@"in"] isEqualToString:@"query"]) {
                [querysWithAuth setObject:authSetting[@"value"] forKey:authSetting[@"key"]];
            }
        }
    }
    
    *headers = [NSDictionary dictionaryWithDictionary:headersWithAuth];
    *querys = [NSDictionary dictionaryWithDictionary:querysWithAuth];
}

#pragma mark - Predicate methods

- (BOOL) isDownloadFile:(NSString *)responseType {
    if ([responseType isEqualToString:@"SWGFile*"]) {
        return YES;
    } else {
        return NO;
    }
}

#pragma mark - Deserialize methods

- (id) deserialize:(id) data class:(NSString *) class {
    NSRegularExpression *regexp = nil;
    NSTextCheckingResult *match = nil;
    NSMutableArray *resultArray = nil;
    NSMutableDictionary *resultDict = nil;

    // return nil if data is nil or class is nil
    if (!data || !class) {
        return nil;
    }

    // remove "*" from class, if ends with "*"
    if ([class hasSuffix:@"*"]) {
        class = [class substringToIndex:[class length] - 1];
    }

    // pure object
    if ([class isEqualToString:@"NSObject"]) {
        return [[NSObject alloc] init];
    }

    // list of models
    NSString *arrayOfModelsPat = @"NSArray<(.+)>";
    regexp = [NSRegularExpression regularExpressionWithPattern:arrayOfModelsPat
                                                      options:NSRegularExpressionCaseInsensitive
                                                        error:nil];
    
    match = [regexp firstMatchInString:class
                               options:0
                                 range:NSMakeRange(0, [class length])];
    
    if (match) {
        NSString *innerType = [class substringWithRange:[match rangeAtIndex:1]];

        resultArray = [NSMutableArray arrayWithCapacity:[data count]];
        [data enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
                [resultArray addObject:[self deserialize:obj class:innerType]];
            }
        ];

        return resultArray;
    }

    // list of primitives
    NSString *arrayOfPrimitivesPet = @"NSArray";
    regexp = [NSRegularExpression regularExpressionWithPattern:arrayOfPrimitivesPet
                                                       options:NSRegularExpressionCaseInsensitive
                                                         error:nil];
    match = [regexp firstMatchInString:class
                               options:0
                                 range:NSMakeRange(0, [class length])];

    if (match) {
        resultArray = [NSMutableArray arrayWithCapacity:[data count]];
        [data enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
            [resultArray addObject:[self deserialize:obj class:NSStringFromClass([obj class])]];
        }];
        
        return resultArray;
    }

    // map
    NSString *dictPat = @"NSDictionary\\* /\\* (.+), (.+) \\*/";
    regexp = [NSRegularExpression regularExpressionWithPattern:dictPat
                                                       options:NSRegularExpressionCaseInsensitive
                                                         error:nil];
    match = [regexp firstMatchInString:class
                               options:0
                                 range:NSMakeRange(0, [class length])];

    if (match) {
        NSString *valueType = [class substringWithRange:[match rangeAtIndex:2]];
        
        resultDict = [NSMutableDictionary dictionaryWithCapacity:[data count]];
        [data enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
            [resultDict setValue:[self deserialize:obj class:valueType] forKey:key];
        }];
        
        return resultDict;
    }
    
    // primitives
    NSArray *primitiveTypes = @[@"NSString", @"NSDate", @"BOOL", @"NSNumber"];

    if ([primitiveTypes containsObject:class]) {
        if ([class isEqualToString:@"NSString"]) {
            return [NSString stringWithString:data];
        }
        else if ([class isEqualToString:@"NSDate"]) {
            return [NSDate dateWithISO8601String:data];
        }
        else if ([class isEqualToString:@"BOOL"]) {
            // Returns YES on encountering one of "Y", "y", "T", "t", or a
            // digit 1-9—the method ignores any trailing characters
            // NSString => BOOL => NSNumber
            return [NSNumber numberWithBool:[data boolValue]];
        }
        else if ([class isEqualToString:@"NSNumber"]) {
            // NSNumber from NSNumber
            if ([data isKindOfClass:[NSNumber class]]) {
                return data;
            }
            // NSNumber from NSString
            else {
                NSNumberFormatter *formatter = [[NSNumberFormatter alloc] init];
                formatter.numberStyle = NSNumberFormatterDecimalStyle;
                return [formatter numberFromString:data];
            }
        }
    }
    
   // model
    Class ModelClass = NSClassFromString(class);
    if ([ModelClass instancesRespondToSelector:@selector(initWithDictionary:error:)]) {
        return [[ModelClass alloc] initWithDictionary:data error:nil];
    }

    return nil;
}

#pragma mark - Operation Methods

- (void) operationWithCompletionBlock: (NSURLRequest *)request
                            requestId: (NSNumber *) requestId
                      completionBlock: (void (^)(id, NSError *))completionBlock {
    AFHTTPRequestOperation *op = [self HTTPRequestOperationWithRequest:request
                                                               success:^(AFHTTPRequestOperation *operation, id response) {
                                                                   if([self executeRequestWithId:requestId]) {
                                                                       if(self.logServerResponses) {
                                                                           [self logResponse:response forRequest:request error:nil];
                                                                       }
                                                                       completionBlock(response, nil);
                                                                   }
                                                               } failure:^(AFHTTPRequestOperation *operation, NSError *error) {
                                                                   if([self executeRequestWithId:requestId]) {
                                                                       NSMutableDictionary *userInfo = [error.userInfo mutableCopy];
                                                                       if(operation.responseObject) {
                                                                           // Add in the (parsed) response body.
                                                                           userInfo[SWGResponseObjectErrorKey] = operation.responseObject;
                                                                       }
                                                                       NSError *augmentedError = [error initWithDomain:error.domain code:error.code userInfo:userInfo];
                                                                       
                                                                       if(self.logServerResponses)
                                                                           [self logResponse:nil forRequest:request error:augmentedError];
                                                                       completionBlock(nil, augmentedError);
                                                                   }
                                                               }];
    
    [self.operationQueue addOperation:op];
}

- (void) downloadOperationWithCompletionBlock: (NSURLRequest *)request
                                    requestId: (NSNumber *) requestId
                              completionBlock: (void (^)(id, NSError *))completionBlock {
    AFHTTPRequestOperation *op = [self HTTPRequestOperationWithRequest:request
                                                               success:^(AFHTTPRequestOperation *operation, id responseObject) {
                                                                   SWGConfiguration *config = [SWGConfiguration sharedConfig];
                                                                   NSString *directory = nil;
                                                                   if (config.tempFolderPath) {
                                                                       directory = config.tempFolderPath;
                                                                   }
                                                                   else {
                                                                       directory = NSTemporaryDirectory();
                                                                   }
                                                                   
                                                                   NSDictionary *headers = operation.response.allHeaderFields;
                                                                   NSString *filename = nil;
                                                                   if ([headers objectForKey:@"Content-Disposition"]) {

                                                                       NSString *pattern = @"filename=['\"]?([^'\"\\s]+)['\"]?";
                                                                       NSRegularExpression *regexp = [NSRegularExpression regularExpressionWithPattern:pattern
                                                                                                                                               options:NSRegularExpressionCaseInsensitive
                                                                                                                                                 error:nil];
                                                                       NSString *contentDispositionHeader = [headers objectForKey:@"Content-Disposition"];
                                                                       NSTextCheckingResult *match = [regexp firstMatchInString:contentDispositionHeader
                                                                                                                        options:0
                                                                                                                          range:NSMakeRange(0, [contentDispositionHeader length])];
                                                                       filename = [contentDispositionHeader substringWithRange:[match rangeAtIndex:1]];
                                                                   }
                                                                   else {
                                                                       filename = [NSString stringWithFormat:@"%@", [[NSProcessInfo processInfo] globallyUniqueString]];
                                                                   }
                                                                   
                                                                   NSString *filepath = [directory stringByAppendingPathComponent:filename];
                                                                   
                                                                   SWGFile *file = [[SWGFile alloc] initWithPath:filepath data:operation.responseData name:filename];
                                                                   completionBlock(file, nil);
                                                               } failure:^(AFHTTPRequestOperation *operation, NSError *error) {
                                                                   
                                                                   if ([self executeRequestWithId:requestId]) {
                                                                       NSMutableDictionary *userInfo = [error.userInfo mutableCopy];
                                                                       if (operation.responseObject) {
                                                                           userInfo[SWGResponseObjectErrorKey] = operation.responseObject;
                                                                       }
                                                                       
                                                                       NSError *augmentedError = [error initWithDomain:error.domain code:error.code userInfo:userInfo];
                                                                       
                                                                       if (self.logServerResponses) {
                                                                           [self logResponse:nil forRequest:request error:augmentedError];
                                                                       }
                                                                       
                                                                       completionBlock(nil, augmentedError);
                                                                   }
                                                               }];
    
    [self.operationQueue addOperation:op];
}

#pragma mark - Perform Request Methods

-(NSNumber*)  requestWithCompletionBlock: (NSString*) path
                                  method: (NSString*) method
                             queryParams: (NSDictionary*) queryParams
                                    body: (id) body
                            headerParams: (NSDictionary*) headerParams
                            authSettings: (NSArray *) authSettings
                      requestContentType: (NSString*) requestContentType
                     responseContentType: (NSString*) responseContentType
                            responseType: (NSString *) responseType
                         completionBlock: (void (^)(id, NSError *))completionBlock {
    // setting request serializer
    if ([requestContentType isEqualToString:@"application/json"]) {
        self.requestSerializer = [SWGJSONRequestSerializer serializer];
    }
    else if ([requestContentType isEqualToString:@"application/x-www-form-urlencoded"]) {
        self.requestSerializer = [AFHTTPRequestSerializer serializer];
    }
    else if ([requestContentType isEqualToString:@"multipart/form-data"]) {
        self.requestSerializer = [AFHTTPRequestSerializer serializer];
    }
    else {
        NSAssert(false, @"unsupport request type %@", requestContentType);
    }

    // setting response serializer
    if ([responseContentType isEqualToString:@"application/json"]) {
        self.responseSerializer = [SWGJSONResponseSerializer serializer];
    }
    else {
        self.responseSerializer = [AFHTTPResponseSerializer serializer];
    }
    
    // auth setting
    [self updateHeaderParams:&headerParams queryParams:&queryParams WithAuthSettings:authSettings];

    NSMutableURLRequest * request = nil;
    if (body != nil && [body isKindOfClass:[NSArray class]]){
        SWGFile * file;
        NSMutableDictionary * params = [[NSMutableDictionary alloc] init];
        for(id obj in body) {
            if([obj isKindOfClass:[SWGFile class]]) {
                file = (SWGFile*) obj;
                requestContentType = @"multipart/form-data";
            }
            else if([obj isKindOfClass:[NSDictionary class]]) {
                for(NSString * key in obj) {
                    params[key] = obj[key];
                }
            }
        }
        NSString * urlString = [[NSURL URLWithString:path relativeToURL:self.baseURL] absoluteString];

        // request with multipart form
        if([requestContentType isEqualToString:@"multipart/form-data"]) {
            request = [self.requestSerializer multipartFormRequestWithMethod: @"POST"
                                                                   URLString: urlString
                                                                  parameters: nil
                                                   constructingBodyWithBlock: ^(id<AFMultipartFormData> formData) {

                                                       for(NSString * key in params) {
                                                           NSData* data = [params[key] dataUsingEncoding:NSUTF8StringEncoding];
                                                           [formData appendPartWithFormData: data name: key];
                                                       }

                                                       if (file) {
                                                           [formData appendPartWithFileData: [file data]
                                                                                       name: [file paramName]
                                                                                   fileName: [file name]
                                                                                   mimeType: [file mimeType]];
                                                       }

                                                   }
                                                                       error:nil];
        }
        // request with form parameters or json
        else {
            NSString* pathWithQueryParams = [self pathWithQueryParamsToString:path queryParams:queryParams];
            NSString* urlString = [[NSURL URLWithString:pathWithQueryParams relativeToURL:self.baseURL] absoluteString];

            request = [self.requestSerializer requestWithMethod:method
                                                      URLString:urlString
                                                     parameters:params
                                                          error:nil];
        }

    }
    else {
        NSString * pathWithQueryParams = [self pathWithQueryParamsToString:path queryParams:queryParams];
        NSString * urlString = [[NSURL URLWithString:pathWithQueryParams relativeToURL:self.baseURL] absoluteString];

        request = [self.requestSerializer requestWithMethod: method
                                                  URLString: urlString
                                                 parameters: body
                                                      error: nil];
    }
  
    BOOL hasHeaderParams = false;
    if(headerParams != nil && [headerParams count] > 0) {
        hasHeaderParams = true;
    }
    if(offlineState) {
        NSLog(@"%@ cache forced", path);
        [request setCachePolicy:NSURLRequestReturnCacheDataDontLoad];
    }
    else if(!hasHeaderParams && [method isEqualToString:@"GET"] && cacheEnabled) {
        NSLog(@"%@ cache enabled", path);
        [request setCachePolicy:NSURLRequestUseProtocolCachePolicy];
    }
    else {
        NSLog(@"%@ cache disabled", path);
        [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    }

    if(hasHeaderParams){
        for(NSString * key in [headerParams keyEnumerator]){
            [request setValue:[headerParams valueForKey:key] forHTTPHeaderField:key];
        }
    }
    [self.requestSerializer setValue:responseContentType forHTTPHeaderField:@"Accept"];


    // Always disable cookies!
    [request setHTTPShouldHandleCookies:NO];

    NSNumber* requestId = [SWGApiClient queueRequest];
    if ([self isDownloadFile:responseType]) {
        [self downloadOperationWithCompletionBlock:request requestId:requestId completionBlock:^(id data, NSError *error) {
            completionBlock(data, error);
        }];
    }
    else {
        [self operationWithCompletionBlock:request requestId:requestId completionBlock:^(id data, NSError *error) {
            completionBlock([self deserialize:data class:responseType], error);
        }];
    }
    return requestId;
}

@end
