#import "Expecta.h"

EXPMatcherInterface(_beGreaterThanOrEqualTo, (id expected));
EXPMatcherInterface(beGreaterThanOrEqualTo, (id expected));

#define beGreaterThanOrEqualTo(expected) _beGreaterThanOrEqualTo(EXPObjectify((expected)))
