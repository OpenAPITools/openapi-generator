#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKNote : NIKSwaggerObject {
@private
    NSString* _noteType; //NSString
    NSArray* _appliesTo; //NSString
    NSString* _value; //NSString
    NSNumber* _pos; //NSNumber
    }



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

