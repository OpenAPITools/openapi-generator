#import <CoreFoundation/CFURL.h>
#import <Foundation/NSError.h>
#import <Foundation/NSJSONSerialization.h>
#import <Foundation/NSURL.h>
#import <Foundation/NSURLError.h>
#import "OMGHTTPURLRQ.h"
#import "OMGUserAgent.h"
#import "OMGFormURLEncode.h"
#import <stdlib.h>

static inline NSMutableURLRequest *OMGMutableURLRequest() {
    NSMutableURLRequest *rq = [NSMutableURLRequest new];
    [rq setValue:OMGUserAgent() forHTTPHeaderField:@"User-Agent"];
    return rq;
}

#define OMGInvalidURLErrorMake() \
    [NSError errorWithDomain:NSURLErrorDomain code:NSURLErrorUnsupportedURL userInfo:@{NSLocalizedDescriptionKey: @"The provided URL was invalid."}]


@implementation OMGMultipartFormData {
@public
    NSString *boundary;
    NSMutableData *body;
}

- (instancetype)init {
    body = [NSMutableData data];
    boundary = [NSString stringWithFormat:@"------------------------%08X%08X", arc4random(), arc4random()];
    return self;
}

- (void)add:(NSData *)payload :(NSString *)name :(NSString *)filename :(NSString *)contentType {
    id ln1 = [NSString stringWithFormat:@"--%@\r\n", boundary];
    id ln2 = ({
        id s = [NSMutableString stringWithString:@"Content-Disposition: form-data; "];
        [s appendFormat:@"name=\"%@\"", name];
        if (filename.length)
            [s appendFormat:@"; filename=\"%@\"", filename];
        [s appendString:@"\r\n"];
        if (contentType.length)
            [s appendFormat:@"Content-Type: %@\r\n", contentType];
        [s appendString:@"\r\n"];
        s;
    });

    [body appendData:[ln1 dataUsingEncoding:NSUTF8StringEncoding]];
    [body appendData:[ln2 dataUsingEncoding:NSUTF8StringEncoding]];
    [body appendData:payload];
    [body appendData:[@"\r\n" dataUsingEncoding:NSUTF8StringEncoding]];
}

- (void)addFile:(NSData *)payload parameterName:(NSString *)name filename:(NSString *)filename contentType:(NSString *)contentType
{
    [self add:payload:name:filename:(contentType ?: @"application/octet-stream")];
}

- (void)addText:(NSString *)text parameterName:(NSString *)parameterName {
    [self add:[text dataUsingEncoding:NSUTF8StringEncoding]:parameterName:nil:nil];
}

- (void)addParameters:(NSDictionary *)parameters {
    for (id key in parameters)
        [self addText:[parameters[key] description] parameterName:key];
}

@end



@implementation OMGHTTPURLRQ

+ (NSMutableURLRequest *)GET:(NSString *)urlString :(NSDictionary *)params error:(NSError **)error {
    id queryString = OMGFormURLEncode(params);
    if (queryString) urlString = [urlString stringByAppendingFormat:@"?%@", queryString];

    id url = [NSURL URLWithString:urlString];
    if (!url) {
        if (error) *error = OMGInvalidURLErrorMake();
        return nil;
    }

    NSMutableURLRequest *rq = OMGMutableURLRequest();
    rq.HTTPMethod = @"GET";
    rq.URL = url;
    return rq;
}

static NSMutableURLRequest *OMGFormURLEncodedRequest(NSString *urlString, NSString *method, NSDictionary *parameters, NSError **error) {
    id url = [NSURL URLWithString:urlString];
    if (!url) {
        if (error) *error = OMGInvalidURLErrorMake();
        return nil;
    }

    NSMutableURLRequest *rq = OMGMutableURLRequest();
    rq.URL = url;
    rq.HTTPMethod = method;
    
    id queryString = OMGFormURLEncode(parameters);
    NSData *data = [queryString dataUsingEncoding:NSUTF8StringEncoding];
    [rq addValue:@"8bit" forHTTPHeaderField:@"Content-Transfer-Encoding"];
    [rq addValue:@"application/x-www-form-urlencoded" forHTTPHeaderField:@"Content-Type"];
    [rq addValue:@(data.length).description forHTTPHeaderField:@"Content-Length"];
    [rq setHTTPBody:data];
    
    return rq;
}

+ (NSMutableURLRequest *)POST:(NSString *)urlString :(id)body error:(NSError **)error {
    if (![body isKindOfClass:[OMGMultipartFormData class]]) {
        return OMGFormURLEncodedRequest(urlString, @"POST", body, error);
    } else {
        id url = [NSURL URLWithString:urlString];
        if (!url) {
            if (error) *error = OMGInvalidURLErrorMake();
            return nil;
        }

        OMGMultipartFormData *multipartFormData = (id)body;
        id const charset = (NSString *)CFStringConvertEncodingToIANACharSetName(CFStringConvertNSStringEncodingToEncoding(NSUTF8StringEncoding));
        id const contentType = [NSString stringWithFormat:@"multipart/form-data; charset=%@; boundary=%@", charset, multipartFormData->boundary];

        NSMutableData *data = [multipartFormData->body mutableCopy];
        id lastLine = [NSString stringWithFormat:@"\r\n--%@--\r\n", multipartFormData->boundary];
        [data appendData:[lastLine dataUsingEncoding:NSUTF8StringEncoding]];

        NSMutableURLRequest *rq = OMGMutableURLRequest();
        [rq setURL:url];
        [rq setHTTPMethod:@"POST"];
        [rq addValue:contentType forHTTPHeaderField:@"Content-Type"];
        [rq setHTTPBody:data];
        return rq;
    }
}

+ (NSMutableURLRequest *)POST:(NSString *)urlString JSON:(id)params error:(NSError **)error {
    if (error) *error = nil;

    id url = [NSURL URLWithString:urlString];
    if (!url) {
        if (error) *error = OMGInvalidURLErrorMake();
        return nil;
    }

    id JSONData = [NSJSONSerialization dataWithJSONObject:params options:(NSJSONWritingOptions)0 error:error];
    if (error && *error) return nil;

    NSMutableURLRequest *rq = OMGMutableURLRequest();
    [rq setURL:url];
    [rq setHTTPMethod:@"POST"];
    [rq setHTTPBody:JSONData];
    [rq setValue:@"application/json; charset=utf-8" forHTTPHeaderField:@"Content-Type"];
    [rq setValue:@"json" forHTTPHeaderField:@"Data-Type"];
    return rq;
}

+ (NSMutableURLRequest *)PUT:(NSString *)url :(NSDictionary *)parameters error:(NSError **)error {
    return OMGFormURLEncodedRequest(url, @"PUT", parameters, error);
}

+ (NSMutableURLRequest *)PUT:(NSString *)url JSON:(id)params error:(NSError **)error {
    NSMutableURLRequest *rq = [OMGHTTPURLRQ POST:url JSON:params error:error];
    rq.HTTPMethod = @"PUT";
    return rq;
}

+ (NSMutableURLRequest *)DELETE:(NSString *)url :(NSDictionary *)parameters error:(NSError **)error {
    return OMGFormURLEncodedRequest(url, @"DELETE", parameters, error);
}

@end
