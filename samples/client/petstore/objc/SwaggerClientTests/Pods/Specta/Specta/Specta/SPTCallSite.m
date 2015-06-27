#import "SPTCallSite.h"

@implementation SPTCallSite

+ (instancetype)callSiteWithFile:(NSString *)file line:(NSUInteger)line {
  return [[self alloc] initWithFile:file line:line];
}

- (instancetype)initWithFile:(NSString *)file line:(NSUInteger)line {
  self = [super init];
  if (self) {
    _file = file;
    _line = line;
  }
  return self;
}

@end
