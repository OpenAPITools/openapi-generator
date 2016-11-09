#import "SWGUser.h"

@implementation SWGUser

- (instancetype)init {
  self = [super init];
  if (self) {
    // initialize property's default value, if any
    
  }
  return self;
}


/**
 * Maps json key to property name.
 * This method is used by `JSONModel`.
 */
+ (JSONKeyMapper *)keyMapper {
  return [[JSONKeyMapper alloc] initWithModelToJSONDictionary:@{ @"_id": @"id", @"username": @"username", @"firstName": @"firstName", @"lastName": @"lastName", @"email": @"email", @"password": @"password", @"phone": @"phone", @"userStatus": @"userStatus" }];
}

/**
 * Indicates whether the property with the given name is optional.
 * If `propertyName` is optional, then return `YES`, otherwise return `NO`.
 * This method is used by `JSONModel`.
 */
+ (BOOL)propertyIsOptional:(NSString *)propertyName {

  NSArray *optionalProperties = @[@"_id", @"username", @"firstName", @"lastName", @"email", @"password", @"phone", @"userStatus"];
  return [optionalProperties containsObject:propertyName];
}

@end
