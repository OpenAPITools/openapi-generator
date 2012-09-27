#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKCitation : NIKSwaggerObject {
@private
    NSString* _cite; //NSString
    NSString* _source; //NSString
    }



@property(nonatomic) NSString* cite;
@property(nonatomic) NSString* source;
- (id) cite: (NSString*) cite
     source: (NSString*) source;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

