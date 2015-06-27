#import "EXPMatchers+beCloseTo.h"
#import "EXPMatcherHelpers.h"

EXPMatcherImplementationBegin(_beCloseToWithin, (id expected, id within)) {
  prerequisite(^BOOL{
    return [actual isKindOfClass:[NSNumber class]] &&
		[expected isKindOfClass:[NSNumber class]] &&
		([within isKindOfClass:[NSNumber class]] || (within == nil));
  });

  match(^BOOL{
		double actualValue = [actual doubleValue];
		double expectedValue = [expected doubleValue];

		if (within != nil) {
			double withinValue = [within doubleValue];
			double lowerBound = expectedValue - withinValue;
			double upperBound = expectedValue + withinValue;
			return (actualValue >= lowerBound) && (actualValue <= upperBound);
		} else {
			double diff = fabs(actualValue - expectedValue);
			actualValue = fabs(actualValue);
			expectedValue = fabs(expectedValue);
			double largest = (expectedValue > actualValue) ? expectedValue : actualValue;
			return (diff <= largest * FLT_EPSILON);
		}
  });

  failureMessageForTo(^NSString *{
    if (within) {
      return [NSString stringWithFormat:@"expected %@ to be close to %@ within %@",
              EXPDescribeObject(actual), EXPDescribeObject(expected), EXPDescribeObject(within)];
    } else {
      return [NSString stringWithFormat:@"expected %@ to be close to %@",
              EXPDescribeObject(actual), EXPDescribeObject(expected)];
    }
  });

  failureMessageForNotTo(^NSString *{
    if (within) {
      return [NSString stringWithFormat:@"expected %@ not to be close to %@ within %@",
              EXPDescribeObject(actual), EXPDescribeObject(expected), EXPDescribeObject(within)];
    } else {
      return [NSString stringWithFormat:@"expected %@ not to be close to %@",
              EXPDescribeObject(actual), EXPDescribeObject(expected)];
    }
  });
}
EXPMatcherImplementationEnd