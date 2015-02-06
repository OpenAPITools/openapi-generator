#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGWordSearchResult.h"


@interface SWGWordSearchResults : SWGObject

@property(nonatomic) NSArray* searchResults;  
@property(nonatomic) NSNumber* totalResults;  
- (id) searchResults: (NSArray*) searchResults     
    totalResults: (NSNumber*) totalResults;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
