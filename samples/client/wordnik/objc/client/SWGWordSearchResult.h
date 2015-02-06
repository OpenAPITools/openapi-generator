#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGWordSearchResult : SWGObject

@property(nonatomic) NSNumber* count;  
@property(nonatomic) NSNumber* lexicality;  
@property(nonatomic) NSString* word;  
- (id) count: (NSNumber*) count     
    lexicality: (NSNumber*) lexicality     
    word: (NSString*) word;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
