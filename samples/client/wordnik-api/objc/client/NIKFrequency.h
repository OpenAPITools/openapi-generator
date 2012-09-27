#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKFrequency : NIKSwaggerObject {
@private
    NSNumber* _count; //NSNumber
    NSNumber* _year; //NSNumber
    }



@property(nonatomic) NSNumber* count;
@property(nonatomic) NSNumber* year;
- (id) count: (NSNumber*) count
     year: (NSNumber*) year;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

