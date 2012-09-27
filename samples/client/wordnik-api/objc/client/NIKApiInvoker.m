#import "NIKApiInvoker.h"

@implementation NIKApiInvoker

@synthesize queue = _queue;
@synthesize defaultHeaders = _defaultHeaders;

- (id) init {
    self = [super init];
    _queue = [[NSOperationQueue alloc] init];
    _defaultHeaders = [[NSMutableDictionary alloc] init];
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

-(id) dictionaryWithCompletionBlock:(NSString*) path
                         method:(NSString*) method
                    queryParams:(NSDictionary*) queryParams
                           body:(id) body
                   headerParams:(NSDictionary*) headerParams
              completionHandler:(void (^)(NSDictionary*, NSError *))completionBlock
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
    NSLog(@"request url: %@", requestUrl);
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    
    NSMutableURLRequest* request = [[NSMutableURLRequest alloc] init];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
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
        else if ([body isKindOfClass:[NSArray class]]){
            data = [NSJSONSerialization dataWithJSONObject:body 
                                                   options:kNilOptions error:&error];
        }
        else {
            data = [body dataUsingEncoding:NSUTF8StringEncoding];
        }
        NSString *postLength = [NSString stringWithFormat:@"%ld", [data length]];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setHTTPBody:data];
        
        [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
        [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
        
        NSLog(@"request: %@", request);
    }
    
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
         }
     }];
    return nil;
}

-(id) stringWithCompletionBlock:(NSString*) path
                             method:(NSString*) method
                        queryParams:(NSDictionary*) queryParams
                               body:(id) body
                       headerParams:(NSDictionary*) headerParams
                  completionHandler:(void (^)(NSString*, NSError *))completionBlock
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
            [requestUrl appendFormat:[NSString stringWithFormat:@"%@%@=%@", separator,
                                      [self escapeString:key], value]];
            counter += 1;
        }
    }
    NSLog(@"request url: %@", requestUrl);
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    
    NSMutableURLRequest* request = [[NSMutableURLRequest alloc] init];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
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
        
        [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
        [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
        
        NSLog(@"request: %@", request);
    }
    
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
         }
     }];
    return nil;
}
@end