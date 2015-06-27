#import "EXPMatchers+beInTheRangeOf.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(_beInTheRangeOf, (id expectedLowerBound, id expectedUpperBound)) {
    match(^BOOL{
        if ([actual respondsToSelector:@selector(compare:)]) {
            NSComparisonResult compareLowerBound = [expectedLowerBound compare: actual];
            NSComparisonResult compareUpperBound = [expectedUpperBound compare: actual];
            if (compareLowerBound == NSOrderedSame) {
                return YES;
            }
            if (compareUpperBound == NSOrderedSame) {
                return YES;
            }
            if ((compareLowerBound == NSOrderedAscending) && (compareUpperBound == NSOrderedDescending)) {
                return YES;
            }
        }
        return NO;
    });

    failureMessageForTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ to be in the range [%@, %@] (inclusive)", EXPDescribeObject(actual), EXPDescribeObject(expectedLowerBound), EXPDescribeObject(expectedUpperBound)];
    });

    failureMessageForNotTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ not to be in the range [%@, %@] (inclusive)", EXPDescribeObject(actual), EXPDescribeObject(expectedLowerBound), EXPDescribeObject(expectedUpperBound)];
    });
}
EXPMatcherImplementationEnd