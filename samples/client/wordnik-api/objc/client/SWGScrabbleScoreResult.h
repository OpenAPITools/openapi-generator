#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGScrabbleScoreResult : SWGObject

@property(nonatomic) NSNumber* value;  

- (id) value: (NSNumber*) value;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

