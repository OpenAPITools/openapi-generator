#import "EXPUnsupportedObject.h"

@implementation EXPUnsupportedObject

@synthesize type=_type;

- (instancetype)initWithType:(NSString *)type {
  self = [super init];
  if(self) {
    self.type = type;
  }
  return self;
}

- (void)dealloc {
  self.type = nil;
  [super dealloc];
}

@end
