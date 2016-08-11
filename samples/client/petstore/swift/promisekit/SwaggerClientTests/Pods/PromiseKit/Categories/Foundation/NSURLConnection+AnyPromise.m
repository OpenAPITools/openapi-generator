#import <CoreFoundation/CFString.h>
#import <CoreFoundation/CFURL.h>
#import <Foundation/NSArray.h>
#import <Foundation/NSError.h>
#import <Foundation/NSJSONSerialization.h>
#import <Foundation/NSOperation.h>
#import <Foundation/NSThread.h>
#import <Foundation/NSURLError.h>
#import <Foundation/NSURL.h>
#import <Foundation/NSURLResponse.h>
#import "NSURLConnection+AnyPromise.h"
#import <PromiseKit/PromiseKit.h>
#import <OMGHTTPURLRQ/OMGHTTPURLRQ.h>
#import <OMGHTTPURLRQ/OMGUserAgent.h>


@implementation NSURLConnection (PromiseKit)

+ (AnyPromise *)GET:(id)urlFormat, ... {
    if ([urlFormat isKindOfClass:[NSString class]]) {
        va_list arguments;
        va_start(arguments, urlFormat);
        urlFormat = [[NSString alloc] initWithFormat:urlFormat arguments:arguments];
        va_end(arguments);
    } else if ([urlFormat isKindOfClass:[NSURL class]]) {
        NSMutableURLRequest *rq = [[NSMutableURLRequest alloc] initWithURL:urlFormat];
        [rq setValue:OMGUserAgent() forHTTPHeaderField:@"User-Agent"];
        return [self promise:rq];
    } else {
        urlFormat = [urlFormat description];
    }
    id err;
    id rq = [OMGHTTPURLRQ GET:urlFormat:nil error:&err];
    if (err) return [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)GET:(NSString *)url query:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ GET:url:params error:&err];
    if (err) return [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)POST:(NSString *)url formURLEncodedParameters:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ POST:url:params error:&err];
    if (err) return [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)POST:(NSString *)urlString JSON:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ POST:urlString JSON:params error:&err];
    if (err) [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)PUT:(NSString *)url formURLEncodedParameters:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ PUT:url:params error:&err];
    if (err) [AnyPromise promiseWithValue:err];
    return [self promise:rq];

}

+ (AnyPromise *)DELETE:(NSString *)url formURLEncodedParameters:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ DELETE:url :params error:&err];
    if (err) [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)promise:(NSURLRequest *)rq {
    static id q = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        q = [NSOperationQueue new];
    });

    return [AnyPromise promiseWithResolverBlock:^(PMKResolver resolve) {
        [NSURLConnection sendAsynchronousRequest:rq queue:q completionHandler:^(id rsp, id data, id urlError) {
            assert(![NSThread isMainThread]);

            PMKResolver fulfiller = ^(id responseObject){
                resolve(PMKManifold(responseObject, rsp, data));
            };
            PMKResolver rejecter = ^(NSError *error){
                id userInfo = error.userInfo.mutableCopy ?: [NSMutableDictionary new];
                if (data) userInfo[PMKURLErrorFailingDataKey] = data;
                if (rsp) userInfo[PMKURLErrorFailingURLResponseKey] = rsp;
                error = [NSError errorWithDomain:error.domain code:error.code userInfo:userInfo];
                resolve(error);
            };

            NSStringEncoding (^stringEncoding)() = ^NSStringEncoding{
                id encodingName = [rsp textEncodingName];
                if (encodingName) {
                    CFStringEncoding encoding = CFStringConvertIANACharSetNameToEncoding((CFStringRef)encodingName);
                    if (encoding != kCFStringEncodingInvalidId)
                        return CFStringConvertEncodingToNSStringEncoding(encoding);
                }
                return NSUTF8StringEncoding;
            };

            if (urlError) {
                rejecter(urlError);
            } else if (![rsp isKindOfClass:[NSHTTPURLResponse class]]) {
                fulfiller(data);
            } else if ([rsp statusCode] < 200 || [rsp statusCode] >= 300) {
                id info = @{
                    NSLocalizedDescriptionKey: @"The server returned a bad HTTP response code",
                    NSURLErrorFailingURLStringErrorKey: rq.URL.absoluteString,
                    NSURLErrorFailingURLErrorKey: rq.URL
                };
                id err = [NSError errorWithDomain:NSURLErrorDomain code:NSURLErrorBadServerResponse userInfo:info];
                rejecter(err);
            } else if (PMKHTTPURLResponseIsJSON(rsp)) {
                // work around ever-so-common Rails workaround: https://github.com/rails/rails/issues/1742
                if ([rsp expectedContentLength] == 1 && [data isEqualToData:[NSData dataWithBytes:" " length:1]])
                    return fulfiller(nil);

                NSError *err = nil;
                id json = [NSJSONSerialization JSONObjectWithData:data options:PMKJSONDeserializationOptions error:&err];
                if (!err) {
                    fulfiller(json);
                } else {
                    id userInfo = err.userInfo.mutableCopy;
                    if (data) {
                        NSString *string = [[NSString alloc] initWithData:data encoding:stringEncoding()];
                        if (string)
                            userInfo[PMKURLErrorFailingStringKey] = string;
                    }
                    long long length = [rsp expectedContentLength];
                    id bytes = length <= 0 ? @"" : [NSString stringWithFormat:@"%lld bytes", length];
                    id fmt = @"The server claimed a %@ JSON response, but decoding failed with: %@";
                    userInfo[NSLocalizedDescriptionKey] = [NSString stringWithFormat:fmt, bytes, userInfo[NSLocalizedDescriptionKey]];
                    err = [NSError errorWithDomain:err.domain code:err.code userInfo:userInfo];
                    rejecter(err);
                }
          #ifdef UIKIT_EXTERN
            } else if (PMKHTTPURLResponseIsImage(rsp)) {
                UIImage *image = [[UIImage alloc] initWithData:data];
                image = [[UIImage alloc] initWithCGImage:[image CGImage] scale:image.scale orientation:image.imageOrientation];
                if (image)
                    fulfiller(image);
                else {
                    id info = @{
                        NSLocalizedDescriptionKey: @"The server returned invalid image data",
                        NSURLErrorFailingURLStringErrorKey: rq.URL.absoluteString,
                        NSURLErrorFailingURLErrorKey: rq.URL
                    };
                    id err = [NSError errorWithDomain:NSURLErrorDomain code:NSURLErrorCannotDecodeContentData userInfo:info];
                    rejecter(err);
                }
          #endif
            } else if (PMKHTTPURLResponseIsText(rsp)) {
                id str = [[NSString alloc] initWithData:data encoding:stringEncoding()];
                if (str)
                    fulfiller(str);
                else {
                    id info = @{
                        NSLocalizedDescriptionKey: @"The server returned invalid string data",
                        NSURLErrorFailingURLStringErrorKey: rq.URL.absoluteString,
                        NSURLErrorFailingURLErrorKey: rq.URL
                    };
                    id err = [NSError errorWithDomain:NSURLErrorDomain code:NSURLErrorCannotDecodeContentData userInfo:info];
                    rejecter(err);
                }
            } else
                fulfiller(data);
        }];
    }];

    #undef fulfiller
}

@end
