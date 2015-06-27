#import "Expecta.h"

EXPMatcherInterface(haveCountOf,   (NSUInteger expected));
EXPMatcherInterface(haveCount,     (NSUInteger expected));
EXPMatcherInterface(haveACountOf,  (NSUInteger expected));
EXPMatcherInterface(haveLength,    (NSUInteger expected));
EXPMatcherInterface(haveLengthOf,  (NSUInteger expected));
EXPMatcherInterface(haveALengthOf, (NSUInteger expected));

#define beEmpty() haveCountOf(0)
