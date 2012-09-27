#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKStringValue : NIKSwaggerObject {
@private
    NSString* _word; //NSString
    }



@property(nonatomic) NSString* word;
- (id) word: (NSString*) word;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

