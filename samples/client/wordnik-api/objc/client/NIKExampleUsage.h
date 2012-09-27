#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKExampleUsage : NIKSwaggerObject {
@private
    NSString* _text; //NSString
    }



@property(nonatomic) NSString* text;
- (id) text: (NSString*) text;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

