#import "Expecta.h"

EXPMatcherInterface(_contain, (id expected));
EXPMatcherInterface(contain, (id expected)); // to aid code completion
#define contain(expected) _contain(EXPObjectify((expected)))
