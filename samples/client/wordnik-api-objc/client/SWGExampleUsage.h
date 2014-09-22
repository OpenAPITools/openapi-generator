#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGExampleUsage : SWGObject

@property(nonatomic) NSString* text;  

- (id) text: (NSString*) text;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

