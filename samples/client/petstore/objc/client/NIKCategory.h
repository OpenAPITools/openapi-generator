#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKCategory : NIKSwaggerObject {
@private
    NSDictionary* raw;
    NSNumber* __id; //NSNumber
    NSString* _name; //NSString
    }

@property(nonatomic) NSDictionary* raw;


@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* name;
- (id) _id: (NSNumber*) _id
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;
- (NSDictionary*) asRaw;


@end

