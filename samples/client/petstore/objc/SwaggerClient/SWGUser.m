#import "SWGUser.h"

@implementation SWGUser

+ (JSONKeyMapper *)keyMapper
{
  return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"username": @"username", @"firstName": @"firstName", @"lastName": @"lastName", @"email": @"email", @"password": @"password", @"phone": @"phone", @"userStatus": @"userStatus" }];
}

+ (BOOL)propertyIsOptional:(NSString *)propertyName
{
  NSArray *optionalProperties = @[@"_id", @"username", @"firstName", @"lastName", @"email", @"password", @"phone", @"userStatus"];

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
