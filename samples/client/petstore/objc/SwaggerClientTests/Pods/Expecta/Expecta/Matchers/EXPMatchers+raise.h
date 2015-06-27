#import "Expecta.h"

EXPMatcherInterface(raise, (NSString *expectedExceptionName));
#define raiseAny() raise(nil)
