#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGNote : SWGObject

@property(nonatomic) NSString* noteType;  

@property(nonatomic) NSArray* appliesTo;  

@property(nonatomic) NSString* value;  

@property(nonatomic) NSNumber* pos;  

- (id) noteType: (NSString*) noteType
     appliesTo: (NSArray*) appliesTo
     value: (NSString*) value
     pos: (NSNumber*) pos;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

