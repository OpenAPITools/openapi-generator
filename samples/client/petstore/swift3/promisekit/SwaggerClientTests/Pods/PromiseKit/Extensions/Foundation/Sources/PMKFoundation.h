#import "NSNotificationCenter+AnyPromise.h"
#import "NSURLSession+AnyPromise.h"

#if TARGET_OS_MAC && !TARGET_OS_EMBEDDED && !TARGET_OS_SIMULATOR
    #import "NSTask+AnyPromise.h"
#endif
