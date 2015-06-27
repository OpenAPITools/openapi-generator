#import "EXPMatchers+conformTo.h"
#import "NSValue+Expecta.h"
#import <objc/runtime.h>

EXPMatcherImplementationBegin(conformTo, (Protocol *expected)) {
    BOOL actualIsNil = (actual == nil);
    BOOL expectedIsNil = (expected == nil);

    prerequisite(^BOOL{
        return !(actualIsNil || expectedIsNil);
    });

    match(^BOOL{
        return [actual conformsToProtocol:expected];
    });

    failureMessageForTo(^NSString *{
        if(actualIsNil) return @"the object is nil/null";
        if(expectedIsNil) return @"the protocol is nil/null";

        NSString *name = NSStringFromProtocol(expected);
        return [NSString stringWithFormat:@"expected: %@ to conform to %@", actual, name];
    });

    failureMessageForNotTo(^NSString *{
        if(actualIsNil) return @"the object is nil/null";
        if(expectedIsNil) return @"the protocol is nil/null";

        NSString *name = NSStringFromProtocol(expected);
        return [NSString stringWithFormat:@"expected: %@ not to conform to %@", actual, name];
    });
}
EXPMatcherImplementationEnd
