#import "Expecta.h"

EXPMatcherInterface(_beLessThanOrEqualTo, (id expected));
EXPMatcherInterface(beLessThanOrEqualTo, (id expected));

#define beLessThanOrEqualTo(expected) _beLessThanOrEqualTo(EXPObjectify((expected)))
