#import "EXPMatchers+beKindOf.h"

EXPMatcherImplementationBegin(beKindOf, (Class expected)) {
  BOOL actualIsNil = (actual == nil);
  BOOL expectedIsNil = (expected == nil);

  prerequisite(^BOOL{
    return !(actualIsNil || expectedIsNil);
  });

  match(^BOOL{
    return [actual isKindOfClass:expected];
  });

  failureMessageForTo(^NSString *{
    if(actualIsNil) return @"the actual value is nil/null";
    if(expectedIsNil) return @"the expected value is nil/null";
    return [NSString stringWithFormat:@"expected: a kind of %@, got: an instance of %@, which is not a kind of %@", [expected class], [actual class], [expected class]];
  });

  failureMessageForNotTo(^NSString *{
    if(actualIsNil) return @"the actual value is nil/null";
    if(expectedIsNil) return @"the expected value is nil/null";
    return [NSString stringWithFormat:@"expected: not a kind of %@, got: an instance of %@, which is a kind of %@", [expected class], [actual class], [expected class]];
  });
}
EXPMatcherImplementationEnd

EXPMatcherAliasImplementation(beAKindOf, beKindOf, (Class expected));
