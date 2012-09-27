#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKWordSearchResult : NIKSwaggerObject {
@private
    NSNumber* _count; //NSNumber
    NSNumber* _lexicality; //NSNumber
    NSString* _word; //NSString
    }



@property(nonatomic) NSNumber* count;
@property(nonatomic) NSNumber* lexicality;
@property(nonatomic) NSString* word;
- (id) count: (NSNumber*) count
     lexicality: (NSNumber*) lexicality
     word: (NSString*) word;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

