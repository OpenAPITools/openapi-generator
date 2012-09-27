#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKFacetValue : NIKSwaggerObject {
@private
    NSNumber* _count; //NSNumber
    NSString* _value; //NSString
    }



@property(nonatomic) NSNumber* count;
@property(nonatomic) NSString* value;
- (id) count: (NSNumber*) count
     value: (NSString*) value;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

