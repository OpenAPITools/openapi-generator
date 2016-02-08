#import <Foundation/NSObjCRuntime.h>
@class NSString;

#if __has_feature(nullability) && defined(NS_ASSUME_NONNULL_BEGIN)
NS_ASSUME_NONNULL_BEGIN
#endif

NSString *OMGUserAgent();

#if __has_feature(nullability) && defined(NS_ASSUME_NONNULL_END)
NS_ASSUME_NONNULL_END
#endif
