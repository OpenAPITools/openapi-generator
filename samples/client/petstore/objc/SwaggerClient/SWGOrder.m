#import "SWGOrder.h"

@implementation SWGOrder
  
+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"petId": @"petId", @"quantity": @"quantity", @"shipDate": @"shipDate", @"status": @"status", @"complete": @"complete" }];
}

+ (BOOL)propertyIsOptional:(NSString *)propertyName
{
  NSArray *optionalProperties = @[@"_id", @"petId", @"quantity", @"shipDate", @"status", @"complete"];

  if ([optionalProperties containsObject:propertyName]) {
    return YES;
  }
  else {
    return NO;
  }
}

@end
