#import "EXPMatchers+equal.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(_equal, (id expected)) {
  match(^BOOL{
    if((actual == expected) || [actual isEqual:expected]) {
      return YES;
    } else if([actual isKindOfClass:[NSNumber class]] && [expected isKindOfClass:[NSNumber class]]) {
      if([actual isKindOfClass:[NSDecimalNumber class]] || [expected isKindOfClass:[NSDecimalNumber class]]) {
        NSDecimalNumber *actualDecimalNumber = [NSDecimalNumber decimalNumberWithDecimal:[(NSNumber *) actual decimalValue]];
        NSDecimalNumber *expectedDecimalNumber = [NSDecimalNumber decimalNumberWithDecimal:[(NSNumber *) expected decimalValue]];
        return [actualDecimalNumber isEqualToNumber:expectedDecimalNumber];
      }
      else {
        if(EXPIsNumberFloat((NSNumber *)actual) || EXPIsNumberFloat((NSNumber *)expected)) {
          return [(NSNumber *)actual floatValue] == [(NSNumber *)expected floatValue];
        }
      }
    }
    return NO;
  });

  failureMessageForTo(^NSString *{
    return [NSString stringWithFormat:@"expected: %@, got: %@", EXPDescribeObject(expected), EXPDescribeObject(actual)];
  });

  failureMessageForNotTo(^NSString *{
    return [NSString stringWithFormat:@"expected: not %@, got: %@", EXPDescribeObject(expected), EXPDescribeObject(actual)];
  });
}
EXPMatcherImplementationEnd
