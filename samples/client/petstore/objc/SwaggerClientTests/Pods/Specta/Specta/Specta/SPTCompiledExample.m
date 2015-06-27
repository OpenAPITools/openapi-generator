#import "SPTCompiledExample.h"

@implementation SPTCompiledExample

- (id)initWithName:(NSString *)name testCaseName:(NSString *)testCaseName block:(SPTSpecBlock)block pending:(BOOL)pending focused:(BOOL)focused {
  self = [super init];
  if (self) {
    self.name = name;
    self.testCaseName = testCaseName;
    self.block = block;
    self.pending = pending;
    self.focused = focused;
  }
  return self;
}

@end
