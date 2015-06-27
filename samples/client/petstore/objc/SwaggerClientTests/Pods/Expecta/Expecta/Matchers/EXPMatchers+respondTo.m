#import "EXPMatchers+respondTo.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(respondTo, (SEL expected)) {
  BOOL actualIsNil = (actual == nil);
  BOOL expectedIsNull = (expected == NULL);

  prerequisite (^BOOL {
    return !(actualIsNil || expectedIsNull);
  });

  match(^BOOL {
    return [actual respondsToSelector:expected];
  });

  failureMessageForTo(^NSString *{
    if (actualIsNil) return @"the object is nil/null";
    if (expectedIsNull) return @"the selector is null";
    return [NSString stringWithFormat:@"expected: %@ to respond to %@", EXPDescribeObject(actual), NSStringFromSelector(expected)];
  });

  failureMessageForNotTo(^NSString *{
    if (actualIsNil) return @"the object is nil/null";
    if (expectedIsNull) return @"the selector is null";
    return [NSString stringWithFormat:@"expected: %@ not to respond to %@", EXPDescribeObject(actual), NSStringFromSelector(expected)];
  });
}
EXPMatcherImplementationEnd
