#import "EXPMatchers+beInstanceOf.h"

EXPMatcherImplementationBegin(beInstanceOf, (Class expected)) {
  BOOL actualIsNil = (actual == nil);
  BOOL expectedIsNil = (expected == nil);

  prerequisite(^BOOL{
    return !(actualIsNil || expectedIsNil);
  });

  match(^BOOL{
    return [actual isMemberOfClass:expected];
  });

  failureMessageForTo(^NSString *{
    if(actualIsNil) return @"the actual value is nil/null";
    if(expectedIsNil) return @"the expected value is nil/null";
    return [NSString stringWithFormat:@"expected: an instance of %@, got: an instance of %@", [expected class], [actual class]];
  });

  failureMessageForNotTo(^NSString *{
    if(actualIsNil) return @"the actual value is nil/null";
    if(expectedIsNil) return @"the expected value is nil/null";
    return [NSString stringWithFormat:@"expected: not an instance of %@, got: an instance of %@", [expected class], [actual class]];
  });
}
EXPMatcherImplementationEnd

EXPMatcherAliasImplementation(beAnInstanceOf, beInstanceOf, (Class expected));
EXPMatcherAliasImplementation(beMemberOf,     beInstanceOf, (Class expected));
EXPMatcherAliasImplementation(beAMemberOf,    beInstanceOf, (Class expected));
