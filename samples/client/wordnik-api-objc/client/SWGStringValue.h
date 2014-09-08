#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGStringValue : SWGObject

@property(nonatomic) NSString* word;  

- (id) word: (NSString*) word;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

