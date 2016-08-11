#import <Foundation/NSObjCRuntime.h>
#import <Foundation/NSString.h>

FOUNDATION_EXPORT double PromiseKitVersionNumber;
FOUNDATION_EXPORT const unsigned char PromiseKitVersionString[];

extern NSString * const PMKErrorDomain;

#define PMKFailingPromiseIndexKey @"PMKFailingPromiseIndexKey"
#define PMKURLErrorFailingURLResponseKey @"PMKURLErrorFailingURLResponseKey"
#define PMKURLErrorFailingDataKey @"PMKURLErrorFailingDataKey"
#define PMKURLErrorFailingStringKey @"PMKURLErrorFailingStringKey"
#define PMKJSONErrorJSONObjectKey @"PMKJSONErrorJSONObjectKey"
#define PMKJoinPromisesKey @"PMKJoinPromisesKey"

#define PMKUnexpectedError 1l
#define PMKUnknownError 2l
#define PMKInvalidUsageError 3l
#define PMKAccessDeniedError 4l
#define PMKOperationCancelled 5l
#define PMKNotFoundError 6l
#define PMKJSONError 7l
#define PMKOperationFailed 8l
#define PMKTaskError 9l
#define PMKJoinError 10l

#if !(defined(PMKEZBake) && defined(SWIFT_CLASS))
    #if !defined(SWIFT_PASTE)
    # define SWIFT_PASTE_HELPER(x, y) x##y
    # define SWIFT_PASTE(x, y) SWIFT_PASTE_HELPER(x, y)
    #endif
    #if !defined(SWIFT_METATYPE)
    # define SWIFT_METATYPE(X) Class
    #endif

    #if defined(__has_attribute) && __has_attribute(objc_runtime_name)
    # define SWIFT_RUNTIME_NAME(X) __attribute__((objc_runtime_name(X)))
    #else
    # define SWIFT_RUNTIME_NAME(X)
    #endif
    #if !defined(SWIFT_CLASS_EXTRA)
    # define SWIFT_CLASS_EXTRA
    #endif
    #if !defined(SWIFT_CLASS)
    # if defined(__has_attribute) && __has_attribute(objc_subclassing_restricted)
    #  define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) __attribute__((objc_subclassing_restricted)) SWIFT_CLASS_EXTRA
    # else
    #  define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) SWIFT_CLASS_EXTRA
    # endif
    #endif

    SWIFT_CLASS("AnyPromise")
    @interface AnyPromise : NSObject
    @property (nonatomic, readonly) BOOL pending;
    @property (nonatomic, readonly) BOOL resolved;
    @property (nonatomic, readonly) BOOL fulfilled;
    @property (nonatomic, readonly) BOOL rejected;
    @end
#endif
