#import <Foundation/Foundation.h>
#import "SWGObject.h"

@interface SWGCategory : SWGObject

@property(nonatomic) NSNumber* _id;  /* Category unique identifier [optional]*/

@property(nonatomic) NSString* name;  /* Name of the category [optional]*/

- (id) _id: (NSNumber*) _id
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

