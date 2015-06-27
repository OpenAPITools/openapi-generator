#import "SPTTestSuite.h"
#import "SPTExampleGroup.h"
#import "SPTCompiledExample.h"

@implementation SPTTestSuite

- (id)init {
  self = [super init];
  if (self) {
    self.rootGroup = [[SPTExampleGroup alloc] init];
    self.rootGroup.root = self.rootGroup;
    self.groupStack = [NSMutableArray arrayWithObject:self.rootGroup];
  }
  return self;
}

- (SPTExampleGroup *)currentGroup {
  return [self.groupStack lastObject];
}

- (void)compile {
  self.compiledExamples = [self.rootGroup compileExamplesWithStack:@[]];
  for (SPTCompiledExample *example in self.compiledExamples) {
    if (example.focused) {
      self.hasFocusedExamples = YES;
      break;
    }
  }
}

@end
