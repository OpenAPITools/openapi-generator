#import <Foundation/Foundation.h>
#import "SWGObject.h"

@interface SWGTag : SWGObject

@property(nonatomic) NSNumber* _id;  /* Unique identifier for the tag [optional]*/

@property(nonatomic) NSString* name;  /* Friendly name for the tag [optional]*/

- (id) _id: (NSNumber*) _id
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

