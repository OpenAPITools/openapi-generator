#import "Expecta.h"

EXPMatcherInterface(_beLessThan, (id expected));
EXPMatcherInterface(beLessThan, (id expected));

#define beLessThan(expected) _beLessThan(EXPObjectify((expected)))
