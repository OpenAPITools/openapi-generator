#import "SWGUser.h"

@implementation SWGUser
  
+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"username": @"username", @"firstName": @"firstName", @"lastName": @"lastName", @"email": @"email", @"password": @"password", @"phone": @"phone", @"userStatus": @"userStatus" }];
}

@end
