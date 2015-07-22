#import "SWGCategory.h"

@implementation SWGCategory

+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"name": @"name" }];
}

+ (BOOL)propertyIsOptional:(NSString *)propertyName
{
  NSArray *optionalProperties = @[@"_id", @"name"];

  if ([optionalProperties containsObject:propertyName]) {
    return YES;
  }
  else {
    return NO;
  }
}

- (NSString *)description {
    return [[self toDictionary] description];
}

@end
