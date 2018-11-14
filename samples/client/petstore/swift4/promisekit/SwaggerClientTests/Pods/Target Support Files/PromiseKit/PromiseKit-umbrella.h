#ifdef __OBJC__
#import <UIKit/UIKit.h>
#else
#ifndef FOUNDATION_EXPORT
#if defined(__cplusplus)
#define FOUNDATION_EXPORT extern "C"
#else
#define FOUNDATION_EXPORT extern
#endif
#endif
#endif

#import "fwd.h"
#import "AnyPromise.h"
#import "PromiseKit.h"

FOUNDATION_EXPORT double PromiseKitVersionNumber;
FOUNDATION_EXPORT const unsigned char PromiseKitVersionString[];

