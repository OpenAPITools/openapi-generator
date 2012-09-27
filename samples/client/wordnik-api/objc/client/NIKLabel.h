#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKLabel : NIKSwaggerObject {
@private
    NSString* _text; //NSString
    NSString* _type; //NSString
    }



@property(nonatomic) NSString* text;
@property(nonatomic) NSString* type;
- (id) text: (NSString*) text
     type: (NSString*) type;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

