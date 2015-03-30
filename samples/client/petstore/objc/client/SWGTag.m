#import "SWGTag.h"

@implementation SWGTag
  
+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"name": @"name" }];
}

@end
