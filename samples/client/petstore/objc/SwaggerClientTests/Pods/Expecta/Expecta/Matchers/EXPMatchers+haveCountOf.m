#import "EXPMatchers+haveCountOf.h"

EXPMatcherImplementationBegin(haveCountOf, (NSUInteger expected)) {
  BOOL actualIsStringy = [actual isKindOfClass:[NSString class]] || [actual isKindOfClass:[NSAttributedString class]];
  BOOL actualIsCompatible = actualIsStringy || [actual respondsToSelector:@selector(count)];

  prerequisite(^BOOL{
    return actualIsCompatible;
  });

  NSUInteger (^count)(id) = ^(id actual) {
    if(actualIsStringy) {
      return [actual length];
  } else {
      return [actual count];
    }
  };

  match(^BOOL{
    if(actualIsCompatible) {
      return count(actual) == expected;
    }
    return NO;
  });

  failureMessageForTo(^NSString *{
    if(!actualIsCompatible) return [NSString stringWithFormat:@"%@ is not an instance of NSString, NSAttributedString, NSArray, NSSet, NSOrderedSet, or NSDictionary", EXPDescribeObject(actual)];
    return [NSString stringWithFormat:@"expected %@ to have a count of %zi but got %zi", EXPDescribeObject(actual), expected, count(actual)];
  });

  failureMessageForNotTo(^NSString *{
    if(!actualIsCompatible) return [NSString stringWithFormat:@"%@ is not an instance of NSString, NSAttributedString, NSArray, NSSet, NSOrderedSet, or NSDictionary", EXPDescribeObject(actual)];
    return [NSString stringWithFormat:@"expected %@ not to have a count of %zi", EXPDescribeObject(actual), expected];
  });
}
EXPMatcherImplementationEnd

EXPMatcherAliasImplementation(haveCount,     haveCountOf, (NSUInteger expected));
EXPMatcherAliasImplementation(haveACountOf,  haveCountOf, (NSUInteger expected));
EXPMatcherAliasImplementation(haveLength,    haveCountOf, (NSUInteger expected));
EXPMatcherAliasImplementation(haveLengthOf,  haveCountOf, (NSUInteger expected));
EXPMatcherAliasImplementation(haveALengthOf, haveCountOf, (NSUInteger expected));
