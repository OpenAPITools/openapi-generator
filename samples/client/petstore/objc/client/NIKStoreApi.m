#import "NIKStoreApi.h"
#import "NIKOrder.h"



@implementation NIKStoreApi
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
 * returnBaseType: NIKOrder
 * returnContainer: 
 * 
 **/
-(void) getOrderByIdWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NIKOrder*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString: [_api escapeString:orderId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(orderId == nil) {
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
        
        completionBlock( [[NIKOrder alloc]initWithValues: data], nil);}];
    
}

/**
 * 
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) deleteOrderWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString: [_api escapeString:orderId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        if(orderId == nil) {
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
 * returnTypeIsPrimitive: true
 * returnBaseType: 
 * returnContainer: 
 * 
 **/
-(void) placeOrderWithCompletionBlock :(NIKOrder*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order", basePath];

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

-(void) getOrderByIdAsJsonWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString: [_api escapeString:orderId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(orderId == nil) {
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

-(void) deleteOrderAsJsonWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString: [_api escapeString:orderId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    if(orderId == nil) {
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

-(void) placeOrderAsJsonWithCompletionBlock :(NIKOrder*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order", basePath];

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


@end
