#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKContentProvider : NIKSwaggerObject {
@private
    NSNumber* __id; //NSNumber
    NSString* _name; //NSString
    }



@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSString* name;
- (id) _id: (NSNumber*) _id
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

