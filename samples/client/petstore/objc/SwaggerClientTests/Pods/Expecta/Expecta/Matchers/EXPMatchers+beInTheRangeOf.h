#import "Expecta.h"

EXPMatcherInterface(_beInTheRangeOf, (id expectedLowerBound, id expectedUpperBound));
EXPMatcherInterface(beInTheRangeOf, (id expectedLowerBound, id expectedUpperBound));

#define beInTheRangeOf(expectedLowerBound, expectedUpperBound) _beInTheRangeOf(EXPObjectify((expectedLowerBound)), EXPObjectify((expectedUpperBound)))
