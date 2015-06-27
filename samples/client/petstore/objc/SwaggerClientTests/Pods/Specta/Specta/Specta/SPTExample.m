#import "SPTExample.h"

@implementation SPTExample

- (id)initWithName:(NSString *)name callSite:(SPTCallSite *)callSite focused:(BOOL)focused block:(SPTVoidBlock)block {
  self = [super init];
  if (self) {
    self.name = name;
    self.callSite = callSite;
    self.block = block;
    self.focused = focused;
    self.pending = block == nil;
  }
  return self;
}

@end
