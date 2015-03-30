#import "SWGPet.h"

@implementation SWGPet
  
+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"category": @"category", @"name": @"name", @"photoUrls": @"photoUrls", @"tags": @"tags", @"status": @"status" }];
}

@end
