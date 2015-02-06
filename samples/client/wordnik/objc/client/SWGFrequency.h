#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGFrequency : SWGObject

@property(nonatomic) NSNumber* count;  
@property(nonatomic) NSNumber* year;  
- (id) count: (NSNumber*) count     
    year: (NSNumber*) year;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
