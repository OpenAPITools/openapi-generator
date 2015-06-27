#import "EXPMatchers+beLessThan.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(_beLessThan, (id expected)) {
    match(^BOOL{
        if ([actual respondsToSelector:@selector(compare:)]) {
            return [actual compare:expected] == NSOrderedAscending;
        }
        return NO;
    });

    failureMessageForTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ to be less than %@", EXPDescribeObject(actual), EXPDescribeObject(expected)];
    });

    failureMessageForNotTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ not to be less than %@", EXPDescribeObject(actual), EXPDescribeObject(expected)];
    });
}
EXPMatcherImplementationEnd