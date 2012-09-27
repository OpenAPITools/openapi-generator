#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKTextPron : NIKSwaggerObject {
@private
    NSString* _raw; //NSString
    NSNumber* _seq; //NSNumber
    NSString* _rawType; //NSString
    }



@property(nonatomic) NSString* raw;
@property(nonatomic) NSNumber* seq;
@property(nonatomic) NSString* rawType;
- (id) raw: (NSString*) raw
     seq: (NSNumber*) seq
     rawType: (NSString*) rawType;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

