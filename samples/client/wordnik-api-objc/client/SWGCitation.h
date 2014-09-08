#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGCitation : SWGObject

@property(nonatomic) NSString* cite;  

@property(nonatomic) NSString* source;  

- (id) cite: (NSString*) cite
     source: (NSString*) source;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

