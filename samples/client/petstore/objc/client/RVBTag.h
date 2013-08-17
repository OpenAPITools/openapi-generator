#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface RVBTag : NIKSwaggerObject

@property(nonatomic) NSString* name;
@property(nonatomic) NSNumber* _id;
- (id) name: (NSString*) name
     _id: (NSNumber*) _id;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

