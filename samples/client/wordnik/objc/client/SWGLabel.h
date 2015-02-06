#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGLabel : SWGObject

@property(nonatomic) NSString* text;  
@property(nonatomic) NSString* type;  
- (id) text: (NSString*) text     
    type: (NSString*) type;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
