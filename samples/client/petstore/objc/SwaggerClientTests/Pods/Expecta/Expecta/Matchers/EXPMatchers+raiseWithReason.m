#import "EXPMatchers+raiseWithReason.h"
#import "EXPDefines.h"

EXPMatcherImplementationBegin(raiseWithReason, (NSString *expectedExceptionName, NSString *expectedReason)) {
    __block NSException *exceptionCaught = nil;
    
    match(^BOOL{
        BOOL expectedExceptionCaught = NO;
        @try {
            ((EXPBasicBlock)actual)();
        } @catch(NSException *e) {
            exceptionCaught = e;
            expectedExceptionCaught = (((expectedExceptionName == nil) || [[exceptionCaught name] isEqualToString:expectedExceptionName]) &&
                                       ((expectedReason == nil) || ([[exceptionCaught reason] isEqualToString:expectedReason])));
        }
        return expectedExceptionCaught;
    });
    
    failureMessageForTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ (%@), got: %@ (%@)",
                expectedExceptionName ?: @"any exception",
                expectedReason ?: @"any reason",
                exceptionCaught ? [exceptionCaught name] : @"no exception",
                exceptionCaught ? [exceptionCaught reason] : @""];
    });
    
    failureMessageForNotTo(^NSString *{
        return [NSString stringWithFormat:@"expected: %@ (%@), got: %@ (%@)",
                expectedExceptionName ? [NSString stringWithFormat:@"not %@", expectedExceptionName] : @"no exception",
                expectedReason ? [NSString stringWithFormat:@"not '%@'", expectedReason] : @"no reason",
                exceptionCaught ? [exceptionCaught name] : @"no exception",
                exceptionCaught ? [exceptionCaught reason] : @"no reason"];
    });
}
EXPMatcherImplementationEnd
