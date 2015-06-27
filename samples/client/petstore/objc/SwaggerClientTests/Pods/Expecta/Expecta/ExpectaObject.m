#import "ExpectaObject.h"

@implementation Expecta

static NSTimeInterval _asynchronousTestTimeout = 1.0;

+ (NSTimeInterval)asynchronousTestTimeout {
  return _asynchronousTestTimeout;
}

+ (void)setAsynchronousTestTimeout:(NSTimeInterval)timeout {
  _asynchronousTestTimeout = timeout;
}

@end