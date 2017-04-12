#import "SWGFakeApi.h"
#import "SWGQueryParamCollection.h"
#import "SWGApiClient.h"


@interface SWGFakeApi ()

@property (nonatomic, strong, readwrite) NSMutableDictionary *mutableDefaultHeaders;

@end

@implementation SWGFakeApi

NSString* kSWGFakeApiErrorDomain = @"SWGFakeApiErrorDomain";
NSInteger kSWGFakeApiMissingParamErrorCode = 234513;

@synthesize apiClient = _apiClient;

#pragma mark - Initialize methods

- (instancetype) init {
    return [self initWithApiClient:[SWGApiClient sharedClient]];
}


-(instancetype) initWithApiClient:(SWGApiClient *)apiClient {
    self = [super init];
    if (self) {
        _apiClient = apiClient;
        _mutableDefaultHeaders = [NSMutableDictionary dictionary];
    }
    return self;
}

#pragma mark -

-(NSString*) defaultHeaderForKey:(NSString*)key {
    return self.mutableDefaultHeaders[key];
}

-(void) setDefaultHeaderValue:(NSString*) value forKey:(NSString*)key {
    [self.mutableDefaultHeaders setValue:value forKey:key];
}

-(NSDictionary *)defaultHeaders {
    return self.mutableDefaultHeaders;
}

#pragma mark - Api Methods

///
/// To test code injection *_/ ' \" =end -- \\r\\n \\n \\r
/// 
///  @param testCodeInjectEndRnNR To test code injection *_/ ' \" =end -- \\r\\n \\n \\r (optional)
///
///  @returns void
///
-(NSURLSessionTask*) testCodeInjectEndRnNRWithTestCodeInjectEndRnNR: (NSString*) testCodeInjectEndRnNR
    completionHandler: (void (^)(NSError* error)) handler {
    NSMutableString* resourcePath = [NSMutableString stringWithFormat:@"/fake"];

    NSMutableDictionary *pathParams = [[NSMutableDictionary alloc] init];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [NSMutableDictionary dictionaryWithDictionary:self.apiClient.configuration.defaultHeaders];
    [headerParams addEntriesFromDictionary:self.defaultHeaders];
    // HTTP header `Accept`
    NSString *acceptHeader = [self.apiClient.sanitizer selectHeaderAccept:@[@"application/json", @"*_/ '  =end --       "]];
    if(acceptHeader.length > 0) {
        headerParams[@"Accept"] = acceptHeader;
    }

    // response content type
    NSString *responseContentType = [[acceptHeader componentsSeparatedByString:@", "] firstObject] ?: @"";

    // request content type
    NSString *requestContentType = [self.apiClient.sanitizer selectHeaderContentType:@[@"application/json", @"*_/ '  =end --       "]];

    // Authentication setting
    NSArray *authSettings = @[];

    id bodyParam = nil;
    NSMutableDictionary *formParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary *localVarFiles = [[NSMutableDictionary alloc] init];
    if (testCodeInjectEndRnNR) {
        formParams[@"test code inject */ &#39; &quot; &#x3D;end -- \r\n \n \r"] = testCodeInjectEndRnNR;
    }

    return [self.apiClient requestWithPath: resourcePath
                                    method: @"PUT"
                                pathParams: pathParams
                               queryParams: queryParams
                                formParams: formParams
                                     files: localVarFiles
                                      body: bodyParam
                              headerParams: headerParams
                              authSettings: authSettings
                        requestContentType: requestContentType
                       responseContentType: responseContentType
                              responseType: nil
                           completionBlock: ^(id data, NSError *error) {
                                if(handler) {
                                    handler(error);
                                }
                            }];
}



@end
