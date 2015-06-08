    
        #import "SWGPet.h"

        @implementation SWGPet

        + (JSONKeyMapper *)keyMapper
        {
        return [[JSONKeyMapper alloc] initWithDictionary:@{ @"id": @"_id", @"category": @"category", @"name": @"name", @"photoUrls": @"photoUrls", @"tags": @"tags", @"status": @"status" }];
        }

        + (BOOL)propertyIsOptional:(NSString *)propertyName
        {
        NSArray *optionalProperties = @[@"_id", @"category", @"tags", @"status"];

        if ([optionalProperties containsObject:propertyName]) {
        return YES;
        }
        else {
        return NO;
        }
        }

    
    @end
