#import "Expecta.h"

EXPMatcherInterface(_beGreaterThan, (id expected));
EXPMatcherInterface(beGreaterThan, (id expected));

#define beGreaterThan(expected) _beGreaterThan(EXPObjectify((expected)))
