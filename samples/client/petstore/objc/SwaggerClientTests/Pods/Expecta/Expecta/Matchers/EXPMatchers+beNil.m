#import "EXPMatchers+beNil.h"

EXPMatcherImplementationBegin(beNil, (void)) {
  match(^BOOL{
    return actual == nil;
  });

  failureMessageForTo(^NSString *{
    return [NSString stringWithFormat:@"expected: nil/null, got: %@", EXPDescribeObject(actual)];
  });

  failureMessageForNotTo(^NSString *{
    return [NSString stringWithFormat:@"expected: not nil/null, got: %@", EXPDescribeObject(actual)];
  });
}
EXPMatcherImplementationEnd

EXPMatcherAliasImplementation(beNull, beNil, (void));
