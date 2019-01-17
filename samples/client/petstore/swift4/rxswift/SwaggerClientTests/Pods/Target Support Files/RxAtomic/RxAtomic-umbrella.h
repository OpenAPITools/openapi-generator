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

#import "RxAtomic.h"

FOUNDATION_EXPORT double RxAtomicVersionNumber;
FOUNDATION_EXPORT const unsigned char RxAtomicVersionString[];

