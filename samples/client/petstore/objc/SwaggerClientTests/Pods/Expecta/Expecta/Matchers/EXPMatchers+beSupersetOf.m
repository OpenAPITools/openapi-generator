#import "EXPMatchers+contain.h"

EXPMatcherImplementationBegin(beSupersetOf, (id subset)) {
  BOOL actualIsCompatible = [actual isKindOfClass:[NSDictionary class]] || [actual respondsToSelector:@selector(containsObject:)];
  BOOL subsetIsNil = (subset == nil);

  // For some instances the isKindOfClass: method returns false, even though
  // they are both actually dictionaries. e.g. Comparing a NSCFDictionary and a
  // NSDictionary.
  // Or in cases when you compare NSMutableArray (which implementation is __NSArrayM:NSMutableArray:NSArray)
  // and NSArray (which implementation is __NSArrayI:NSArray)
  BOOL bothAreIdenticalCollectionClasses = ([actual isKindOfClass:[NSDictionary class]] && [subset isKindOfClass:[NSDictionary class]]) ||
        ([actual isKindOfClass:[NSArray class]] && [subset isKindOfClass:[NSArray class]]) ||
        ([actual isKindOfClass:[NSSet class]] && [subset isKindOfClass:[NSSet class]]) ||
        ([actual isKindOfClass:[NSOrderedSet class]] && [subset isKindOfClass:[NSOrderedSet class]]);

  BOOL classMatches = bothAreIdenticalCollectionClasses || [subset isKindOfClass:[actual class]];

  prerequisite(^BOOL{
    return actualIsCompatible && !subsetIsNil && classMatches;
  });

  match(^BOOL{
    if(!actualIsCompatible) return NO;

    if([actual isKindOfClass:[NSDictionary class]]) {
      for (id key in subset) {
        id actualValue = [actual valueForKey:key];
        id subsetValue = [subset valueForKey:key];

        if (![subsetValue isEqual:actualValue]) return NO;
      }
    } else {
      for (id object in subset) {
        if (![actual containsObject:object]) return NO;
      }
    }

    return YES;
  });

  failureMessageForTo(^NSString *{
    if(!actualIsCompatible) return [NSString stringWithFormat:@"%@ is not an instance of NSDictionary and does not implement -containsObject:", EXPDescribeObject(actual)];

    if(subsetIsNil) return @"the expected value is nil/null";

    if(!classMatches) return [NSString stringWithFormat:@"%@ does not match the class of %@", EXPDescribeObject(subset), EXPDescribeObject(actual)];

    return [NSString stringWithFormat:@"expected %@ to be a superset of %@", EXPDescribeObject(actual), EXPDescribeObject(subset)];
  });

  failureMessageForNotTo(^NSString *{
    if(!actualIsCompatible) return [NSString stringWithFormat:@"%@ is not an instance of NSDictionary and does not implement -containsObject:", EXPDescribeObject(actual)];

    if(subsetIsNil) return @"the expected value is nil/null";

    if(!classMatches) return [NSString stringWithFormat:@"%@ does not match the class of %@", EXPDescribeObject(subset), EXPDescribeObject(actual)];

    return [NSString stringWithFormat:@"expected %@ not to be a superset of %@", EXPDescribeObject(actual), EXPDescribeObject(subset)];
  });
}
EXPMatcherImplementationEnd
