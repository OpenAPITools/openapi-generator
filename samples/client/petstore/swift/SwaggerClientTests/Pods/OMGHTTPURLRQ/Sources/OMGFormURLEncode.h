#import <Foundation/NSDictionary.h>
#import <Foundation/NSObjCRuntime.h>
#import <Foundation/NSString.h>

#if __has_feature(nullability) && defined(NS_ASSUME_NONNULL_BEGIN)
NS_ASSUME_NONNULL_BEGIN
#endif

/**
 Express this dictionary as a `application/x-www-form-urlencoded` string.

 Most users would recognize the result of this transformation as the query
 string in a browser bar. For our purposes it is the query string in a GET
 request and the HTTP body for POST, PUT and DELETE requests.

 If the parameters dictionary is nil or empty, returns nil.
*/
NSString *OMGFormURLEncode(NSDictionary *parameters);

#if __has_feature(nullability) && defined(NS_ASSUME_NONNULL_END)
NS_ASSUME_NONNULL_END
#endif
