#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGTextPron : SWGObject

@property(nonatomic) NSString* raw;  
@property(nonatomic) NSNumber* seq;  
@property(nonatomic) NSString* rawType;  
- (id) raw: (NSString*) raw     
    seq: (NSNumber*) seq     
    rawType: (NSString*) rawType;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
