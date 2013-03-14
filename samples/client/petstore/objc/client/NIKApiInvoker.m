#import "NIKApiInvoker.h"
#import "NIKFile.h"

@implementation NIKApiInvoker

@synthesize queue = _queue;
@synthesize defaultHeaders = _defaultHeaders;


static NSInteger __LoadingObjectsCount = 0;

+ (NIKApiInvoker*)sharedInstance {
    static NIKApiInvoker *_sharedInstance = nil;
    if (!_sharedInstance) {
        _sharedInstance = [[NIKApiInvoker alloc] init];
    }
    return _sharedInstance;
}

- (void)updateLoadCountWithDelta:(NSInteger)countDelta {
    @synchronized(self) {
        __LoadingObjectsCount += countDelta;
        __LoadingObjectsCount = (__LoadingObjectsCount < 0) ? 0 : __LoadingObjectsCount ;
        
#if TARGET_OS_IPHONE
        [UIApplication sharedApplication].networkActivityIndicatorVisible = __LoadingObjectsCount > 0;
#endif
    }
}

- (void)startLoad {
    [self updateLoadCountWithDelta:1];
}

- (void)stopLoad {
    [self updateLoadCountWithDelta:-1];
}


- (id) init {
    self = [super init];
    _queue = [[NSOperationQueue alloc] init];
    _defaultHeaders = [[NSMutableDictionary alloc] init];
    _cachePolicy = NSURLRequestUseProtocolCachePolicy;
    return self;
}

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key {
    [_defaultHeaders setValue:value forKey:key];
}

-(NSString*) escapeString:(NSString *)unescaped {
    return (NSString *)CFBridgingRelease(CFURLCreateStringByAddingPercentEscapes(
                                                                                 NULL,
                                                                                 (__bridge CFStringRef) unescaped,
                                                                                 NULL,
                                                                                 (CFStringRef)@"!*'();:@&=+$,/?%#[]",
                                                                                 kCFStringEncodingUTF8));
}

-(void) dictionary: (NSString*) path
            method: (NSString*) method
       queryParams: (NSDictionary*) queryParams
              body: (id) body
      headerParams: (NSDictionary*) headerParams
       contentType: (NSString*) contentType
   completionBlock: (void (^)(NSDictionary*, NSError *))completionBlock
{
    NSMutableString * requestUrl = [NSMutableString stringWithFormat:@"%@", path];
    NSString * separator = nil;
    int counter = 0;
    if(queryParams != nil){
        for(NSString * key in [queryParams keyEnumerator]){
            if(counter == 0) separator = @"?";
            else separator = @"&";
            NSString * value;
            if([[queryParams valueForKey:key] isKindOfClass:[NSString class]]){
                value = [self escapeString:[queryParams valueForKey:key]];
            }
            else {
                value = [NSString stringWithFormat:@"%@", [queryParams valueForKey:key]];
            }
            [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                                      [self escapeString:key], value]];
            counter += 1;
        }
    }
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"RVBLogging"]) {
        NSLog(@"request url: %@", requestUrl);
    }
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    
    NSMutableURLRequest* request = [[NSMutableURLRequest alloc] init];
    [request setURL:URL];
    [request setCachePolicy:self.cachePolicy];
    [request setTimeoutInterval:30];
    
    for(NSString * key in [_defaultHeaders keyEnumerator]){
        [request setValue:[_defaultHeaders valueForKey:key] forHTTPHeaderField:key];
    }
    if(headerParams != nil){
        for(NSString * key in [headerParams keyEnumerator]){
            [request setValue:[headerParams valueForKey:key] forHTTPHeaderField:key];
        }
    }
    [request setHTTPMethod:method];
    if(body != nil) {
        NSError * error = [NSError new];
        NSData * data = nil;
        if([body isKindOfClass:[NSDictionary class]]){
            data = [NSJSONSerialization dataWithJSONObject:body
                                                   options:kNilOptions error:&error];
        }
        else if ([body isKindOfClass:[NIKFile class]]){
            NIKFile * file = (NIKFile*) body;

            NSString *boundary = @"Fo0+BAr";
            contentType = [NSString stringWithFormat:@"multipart/form-data; boundary=%@", boundary];

            // add the body
            NSMutableData *postBody = [NSMutableData data];
            [postBody appendData:[[NSString stringWithFormat:@"--%@\r\n", boundary] dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData:[@"Content-Disposition: form-data; name= \"some_name\"\r\n\r\n" dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData:[[NSString stringWithFormat:@"Content-Disposition: form-data; name=\"image_file\"; filename=\"%@\"\r\n", file] dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData:[[NSString stringWithFormat:@"Content-Type: %@\r\n\r\n", file.mimeType] dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData: file.data];
            [postBody appendData:[[NSString stringWithFormat:@"\r\n--%@--\r\n",boundary] dataUsingEncoding:NSUTF8StringEncoding]];
        }
        else if ([body isKindOfClass:[NSArray class]]){
            data = [NSJSONSerialization dataWithJSONObject:body
                                                   options:kNilOptions error:&error];
        }
        else {
            data = [body dataUsingEncoding:NSUTF8StringEncoding];
        }
        NSString *postLength = [NSString stringWithFormat:@"%d", [data length]];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setHTTPBody:data];
        
        [request setValue:contentType forHTTPHeaderField:@"Content-Type"];
    }
    
    // Handle caching on GET requests
    if ((_cachePolicy == NSURLRequestReturnCacheDataElseLoad || _cachePolicy == NSURLRequestReturnCacheDataDontLoad) && [method isEqualToString:@"GET"]) {
        NSCachedURLResponse *cacheResponse = [[NSURLCache sharedURLCache] cachedResponseForRequest:request];
        NSData *data = [cacheResponse data];
        if (data) {
            NSError *error = nil;
            NSDictionary* results = [NSJSONSerialization JSONObjectWithData:data
                                                                    options:kNilOptions
                                                                      error:&error];
            completionBlock(results, nil);
        }
    }

    if (_cachePolicy == NSURLRequestReturnCacheDataDontLoad)
        return;
    
    [self startLoad];
    NSDate *date = [NSDate date];
    [NSURLConnection sendAsynchronousRequest:request queue:_queue completionHandler:
     ^(NSURLResponse *response, NSData *data, NSError *error) {
         long statusCode = [(NSHTTPURLResponse*)response statusCode];
         
         if (error) {
             completionBlock(nil, error);
             return;
         }
         else if (!NSLocationInRange(statusCode, NSMakeRange(200, 99))){
             error = [NSError errorWithDomain:@"swagger"
                                         code:statusCode
                                     userInfo:[NSJSONSerialization JSONObjectWithData:data
                                                                              options:kNilOptions
                                                                                error:&error]];
             completionBlock(nil, error);
             return;
         }
         else {
             NSDictionary* results = [NSJSONSerialization JSONObjectWithData:data
                                                                     options:kNilOptions
                                                                       error:&error];
             completionBlock(results, nil);
             if ([[NSUserDefaults standardUserDefaults] boolForKey:@"RVBLogging"]) {
                 NSLog(@"fetched results (%f seconds): %@", [[NSDate date] timeIntervalSinceDate:date], results);
             }
         }
         
         [self stopLoad];
     }];
}

-(void) stringWithCompletionBlock: (NSString*) path
                           method: (NSString*) method
                      queryParams: (NSDictionary*) queryParams
                             body: (id) body
                     headerParams: (NSDictionary*) headerParams
                      contentType: (NSString*) contentType
                  completionBlock: (void (^)(NSString*, NSError *))completionBlock
{
    NSMutableString * requestUrl = [NSMutableString stringWithFormat:@"%@", path];
    NSString * separator = nil;
    int counter = 0;
    if(queryParams != nil){
        for(NSString * key in [queryParams keyEnumerator]){
            if(counter == 0) separator = @"?";
            else separator = @"&";
            NSString * value;
            if([[queryParams valueForKey:key] isKindOfClass:[NSString class]]){
                value = [self escapeString:[queryParams valueForKey:key]];
            }
            else {
                value = [NSString stringWithFormat:@"%@", [queryParams valueForKey:key]];
            }
            [requestUrl appendString:[NSString stringWithFormat:@"%@%@=%@", separator,
                                      [self escapeString:key], value]];
            counter += 1;
        }
    }
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"RVBLogging"]) {
        NSLog(@"request url: %@", requestUrl);
    }
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    
    NSMutableURLRequest* request = [[NSMutableURLRequest alloc] init];
    [request setURL:URL];
    [request setCachePolicy:self.cachePolicy];
    [request setTimeoutInterval:30];
    
    for(NSString * key in [_defaultHeaders keyEnumerator]){
        [request setValue:[_defaultHeaders valueForKey:key] forHTTPHeaderField:key];
    }
    if(headerParams != nil){
        for(NSString * key in [headerParams keyEnumerator]){
            [request setValue:[headerParams valueForKey:key] forHTTPHeaderField:key];
        }
    }
    [request setHTTPMethod:method];
    if(body != nil) {
        NSError * error = [NSError new];
        NSData * data = nil;
        if([body isKindOfClass:[NSDictionary class]]){
            data = [NSJSONSerialization dataWithJSONObject:body
                                                   options:kNilOptions error:&error];
        }
        else if ([body isKindOfClass:[NIKFile class]]){
            NIKFile * file = (NIKFile*) body;
            
            NSString *boundary = @"Fo0+BAr";
            contentType = [NSString stringWithFormat:@"multipart/form-data; boundary=%@", boundary];
            
            // add the body
            NSMutableData *postBody = [NSMutableData data];
            [postBody appendData:[[NSString stringWithFormat:@"--%@\r\n", boundary] dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData:[@"Content-Disposition: form-data; name= \"some_name\"\r\n\r\n" dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData:[[NSString stringWithFormat:@"Content-Disposition: form-data; name=\"image_file\"; filename=\"%@\"\r\n", file] dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData:[[NSString stringWithFormat:@"Content-Type: %@\r\n\r\n", file.mimeType] dataUsingEncoding:NSUTF8StringEncoding]];
            [postBody appendData: file.data];
            [postBody appendData:[[NSString stringWithFormat:@"\r\n--%@--\r\n",boundary] dataUsingEncoding:NSUTF8StringEncoding]];
        }
        else if ([body isKindOfClass:[NSArray class]]){
            data = [NSJSONSerialization dataWithJSONObject:body
                                                   options:kNilOptions error:&error];
        }
        else {
            data = [body dataUsingEncoding:NSUTF8StringEncoding];
        }
        NSString *postLength = [NSString stringWithFormat:@"%d", [data length]];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setHTTPBody:data];
        
        [request setValue:contentType forHTTPHeaderField:@"Content-Type"];
    }
    
    
    // Handle caching on GET requests
    if ((_cachePolicy == NSURLRequestReturnCacheDataElseLoad || _cachePolicy == NSURLRequestReturnCacheDataDontLoad) && [method isEqualToString:@"GET"]) {
        NSCachedURLResponse *cacheResponse = [[NSURLCache sharedURLCache] cachedResponseForRequest:request];
        NSData *data = [cacheResponse data];
        if (data) {
            NSString* results = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
            
            if(results && [results length] >= 2) {
                if(([results characterAtIndex:0] == '\"') && ([results characterAtIndex:([results length] - 1) == '\"'])){
                    results = [results substringWithRange:NSMakeRange(1, [results length] -2)];
                }
            }
            completionBlock(results, nil);
            if ([[NSUserDefaults standardUserDefaults] boolForKey:@"RVBLogging"]) {
                //   NSLog(@"cached results: %@", results);
            }
        }
        
    }
    
    if (_cachePolicy == NSURLRequestReturnCacheDataDontLoad)
        return;
    
    [self startLoad];
    NSDate *date = [NSDate date];
    [NSURLConnection sendAsynchronousRequest:request queue:_queue completionHandler:
     ^(NSURLResponse *response, NSData *data, NSError *error) {
         int statusCode = [(NSHTTPURLResponse*)response statusCode];
         if (error) {
             completionBlock(nil, error);
             return;
         }
         else if (!NSLocationInRange(statusCode, NSMakeRange(200, 99))){
             error = [NSError errorWithDomain:@"swagger"
                                         code:statusCode
                                     userInfo:[NSJSONSerialization JSONObjectWithData:data
                                                                              options:kNilOptions
                                                                                error:&error]];
             completionBlock(nil, error);
             return;
         }
         else {
             NSString* results = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
             
             if(results && [results length] >= 2) {
                 if(([results characterAtIndex:0] == '\"') && ([results characterAtIndex:([results length] - 1) == '\"'])){
                     results = [results substringWithRange:NSMakeRange(1, [results length] -2)];
                 }
                 
             }
             completionBlock(results, nil);
             if ([[NSUserDefaults standardUserDefaults] boolForKey:@"RVBLogging"]) {
                 NSLog(@"fetched results (%f seconds): %@", [[NSDate date] timeIntervalSinceDate:date], results);
             }
         }
         [self stopLoad];
     }];
}
@end