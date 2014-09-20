#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGSyllable : SWGObject

@property(nonatomic) NSString* text;  

@property(nonatomic) NSNumber* seq;  

@property(nonatomic) NSString* type;  

- (id) text: (NSString*) text
     seq: (NSNumber*) seq
     type: (NSString*) type;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

