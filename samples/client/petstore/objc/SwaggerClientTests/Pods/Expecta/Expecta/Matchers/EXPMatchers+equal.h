#import "Expecta.h"

EXPMatcherInterface(_equal, (id expected));
EXPMatcherInterface(equal, (id expected)); // to aid code completion
#define equal(...) _equal(EXPObjectify((__VA_ARGS__)))
