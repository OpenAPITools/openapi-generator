#import "EXPMatchers+beGreaterThanOrEqualTo.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(_beGreaterThanOrEqualTo, (id expected)) {
    match(^BOOL{
        if ([actual respondsToSelector:@selector(compare:)]) {
            return [actual compare:expected] != NSOrderedAscending;
        }
        return NO;
    });

    failureMessageForTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ to be greater than or equal to %@", EXPDescribeObject(actual), EXPDescribeObject(expected)];
    });

    failureMessageForNotTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ not to be greater than or equal to %@", EXPDescribeObject(actual), EXPDescribeObject(expected)];
    });
}
EXPMatcherImplementationEnd