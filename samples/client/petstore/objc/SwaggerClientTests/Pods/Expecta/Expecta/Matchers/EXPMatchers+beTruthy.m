#import "EXPMatchers+beTruthy.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(beTruthy, (void)) {
  match(^BOOL{
    if([actual isKindOfClass:[NSNumber class]]) {
      return !![(NSNumber *)actual boolValue];
    } else if([actual isKindOfClass:[NSValue class]]) {
      if(EXPIsValuePointer((NSValue *)actual)) {
        return !![(NSValue *)actual pointerValue];
      }
    }
    return !!actual;
  });

  failureMessageForTo(^NSString *{
    return [NSString stringWithFormat:@"expected: a truthy value, got: %@, which is falsy", EXPDescribeObject(actual)];
  });

  failureMessageForNotTo(^NSString *{
    return [NSString stringWithFormat:@"expected: a non-truthy value, got: %@, which is truthy", EXPDescribeObject(actual)];
  });
}
EXPMatcherImplementationEnd
