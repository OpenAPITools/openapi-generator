#import "SWGOrder.h"

@implementation SWGOrder
  
+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"petId": @"petId", @"quantity": @"quantity", @"shipDate": @"shipDate", @"status": @"status", @"complete": @"complete" }];
}

@end
