#import "EXPMatchers+match.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(match, (NSString *expected)) {
  BOOL actualIsNil = (actual == nil);
  BOOL expectedIsNil = (expected == nil);
  
  __block NSRegularExpression *regex = nil;
  __block NSError *regexError = nil;
  
  prerequisite (^BOOL {
    BOOL nilInput = (actualIsNil || expectedIsNil);
    if (!nilInput) {
      regex = [NSRegularExpression regularExpressionWithPattern:expected options:0 error:&regexError];
    }
    return !nilInput && regex;
  });
  
  match(^BOOL {
    NSRange range = [regex rangeOfFirstMatchInString:actual options:0 range:NSMakeRange(0, [actual length])];
    return !NSEqualRanges(range, NSMakeRange(NSNotFound, 0));
  });
  
  failureMessageForTo(^NSString *{
    if (actualIsNil) return @"the object is nil/null";
    if (expectedIsNil) return @"the expression is nil/null";
    if (regexError) return [NSString stringWithFormat:@"unable to create regular expression from given parameter: %@", [regexError localizedDescription]];
    return [NSString stringWithFormat:@"expected: %@ to match to %@", EXPDescribeObject(actual), expected];
  });
  
  failureMessageForNotTo(^NSString *{
    if (actualIsNil) return @"the object is nil/null";
    if (expectedIsNil) return @"the expression is nil/null";
    if (regexError) return [NSString stringWithFormat:@"unable to create regular expression from given parameter: %@", [regexError localizedDescription]];
    return [NSString stringWithFormat:@"expected: %@ not to match to %@", EXPDescribeObject(actual), expected];
  });
}
EXPMatcherImplementationEnd
