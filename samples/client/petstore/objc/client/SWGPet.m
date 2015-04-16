#import "SWGPet.h"

@implementation SWGPet
  
+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"category": @"category", @"name": @"name", @"photoUrls": @"photoUrls", @"tags": @"tags", @"status": @"status" }];
}

+ (BOOL)propertyIsOptional:(NSString *)propertyName
{
  NSArray *optionalProperties = @[@"category", @"tags", @"status", @"photoUrls", @"_id"];

  if ([optionalProperties containsObject:propertyName]) {
    return YES;
  }
  else {
    return NO;
  }
}

@end
