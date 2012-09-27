#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKSyllable : NIKSwaggerObject {
@private
    NSString* _text; //NSString
    NSNumber* _seq; //NSNumber
    NSString* _type; //NSString
    }



@property(nonatomic) NSString* text;
@property(nonatomic) NSNumber* seq;
@property(nonatomic) NSString* type;
- (id) text: (NSString*) text
     seq: (NSNumber*) seq
     type: (NSString*) type;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

