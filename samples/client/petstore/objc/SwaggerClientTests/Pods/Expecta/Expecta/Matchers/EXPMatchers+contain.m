#import "EXPMatchers+contain.h"

EXPMatcherImplementationBegin(_contain, (id expected)) {
  BOOL actualIsCompatible = [actual isKindOfClass:[NSString class]] || [actual conformsToProtocol:@protocol(NSFastEnumeration)];
  BOOL expectedIsNil = (expected == nil);

  prerequisite(^BOOL{
    return actualIsCompatible && !expectedIsNil;
  });

  match(^BOOL{
    if(actualIsCompatible) {
      if([actual isKindOfClass:[NSString class]]) {
        return [(NSString *)actual rangeOfString:[expected description]].location != NSNotFound;
      } else {
        for (id object in actual) {
          if ([object isEqual:expected]) {
            return YES;
          }
        }
      }
    }
    return NO;
  });

  failureMessageForTo(^NSString *{
    if(!actualIsCompatible) return [NSString stringWithFormat:@"%@ is not an instance of NSString or NSFastEnumeration", EXPDescribeObject(actual)];
    if(expectedIsNil) return @"the expected value is nil/null";
    return [NSString stringWithFormat:@"expected %@ to contain %@", EXPDescribeObject(actual), EXPDescribeObject(expected)];
  });

  failureMessageForNotTo(^NSString *{
    if(!actualIsCompatible) return [NSString stringWithFormat:@"%@ is not an instance of NSString or NSFastEnumeration", EXPDescribeObject(actual)];
    if(expectedIsNil) return @"the expected value is nil/null";
    return [NSString stringWithFormat:@"expected %@ not to contain %@", EXPDescribeObject(actual), EXPDescribeObject(expected)];
  });
}
EXPMatcherImplementationEnd
