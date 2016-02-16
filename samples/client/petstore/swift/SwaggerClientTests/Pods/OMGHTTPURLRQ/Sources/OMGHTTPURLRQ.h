#import <Foundation/NSObjCRuntime.h>

FOUNDATION_EXPORT double OMGHTTPURLRQVersionNumber;
FOUNDATION_EXPORT const unsigned char OMGHTTPURLRQVersionString[];

#import <Foundation/NSData.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSURLRequest.h>
#import <Foundation/NSString.h>
#import "OMGFormURLEncode.h"
#import "OMGUserAgent.h"


#if __has_feature(nullability) && defined(NS_ASSUME_NONNULL_BEGIN)
NS_ASSUME_NONNULL_BEGIN
#else
#define nullable
#endif

/**
 The error will either be a JSON error (NSCocoaDomain :/) or in the NSURLErrorDomain
 with code: NSURLErrorUnsupportedURL.
*/
@interface OMGHTTPURLRQ : NSObject

+ (nullable NSMutableURLRequest *)GET:(NSString *)url :(nullable NSDictionary *)parameters error:(NSError **)error;
+ (nullable NSMutableURLRequest *)POST:(NSString *)url :(nullable id)parametersOrMultipartFormData error:(NSError **)error;
+ (nullable NSMutableURLRequest *)POST:(NSString *)url JSON:(nullable id)JSONObject error:(NSError **)error;
+ (nullable NSMutableURLRequest *)PUT:(NSString *)url :(nullable NSDictionary *)parameters error:(NSError **)error;
+ (nullable NSMutableURLRequest *)PUT:(NSString *)url JSON:(nullable id)JSONObject error:(NSError **)error;
+ (nullable NSMutableURLRequest *)DELETE:(NSString *)url :(nullable NSDictionary *)parameters error:(NSError **)error;

@end


/**
 POST this with `OMGHTTPURLRQ`’s `-POST::` class method.
*/
@interface OMGMultipartFormData : NSObject

/**
 The `filename` parameter is optional. The content-type is optional, and 
 if left `nil` will default to *octet-stream*.
*/
- (void)addFile:(NSData *)data parameterName:(NSString *)parameterName filename:(nullable NSString *)filename contentType:(nullable NSString *)contentType;

- (void)addText:(NSString *)text parameterName:(NSString *)parameterName;

/**
 Technically adding parameters to a multipart/form-data request is abusing
 the specification. What we do is add each parameter as a text-item. Any
 API that expects parameters in a multipart/form-data request will expect
 the parameters to be encoded in this way.
*/
- (void)addParameters:(NSDictionary *)parameters;

@end


#if __has_feature(nullability) && defined(NS_ASSUME_NONNULL_END)
NS_ASSUME_NONNULL_END
#else
#undef nullable
#endif
