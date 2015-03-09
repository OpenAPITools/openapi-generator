#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGApiResponse : SWGObject

@property(nonatomic) NSNumber* code;  
@property(nonatomic) NSString* type;  
@property(nonatomic) NSString* message;  
- (id) code: (NSNumber*) code     
    type: (NSString*) type     
    message: (NSString*) message;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
