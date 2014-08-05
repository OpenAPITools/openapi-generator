#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGFacetValue : SWGObject

@property(nonatomic) NSNumber* count;  

@property(nonatomic) NSString* value;  

- (id) count: (NSNumber*) count
     value: (NSString*) value;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

