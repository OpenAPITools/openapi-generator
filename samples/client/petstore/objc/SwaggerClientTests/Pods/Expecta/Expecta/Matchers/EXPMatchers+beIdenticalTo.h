#import "Expecta.h"

EXPMatcherInterface(_beIdenticalTo, (void *expected));
EXPMatcherInterface(beIdenticalTo, (void *expected)); // to aid code completion

#if __has_feature(objc_arc)
#define beIdenticalTo(expected) _beIdenticalTo((__bridge void*)expected)
#else
#define beIdenticalTo(expected) _beIdenticalTo(expected)
#endif
