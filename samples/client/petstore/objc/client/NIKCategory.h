#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKCategory : NIKSwaggerObject

@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* name;
- (id) _id: (NSNumber*) _id
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

