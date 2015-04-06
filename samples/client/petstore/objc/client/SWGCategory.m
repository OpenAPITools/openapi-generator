#import "SWGCategory.h"

@implementation SWGCategory
  
+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"name": @"name" }];
}

@end
