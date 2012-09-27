#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKWordSearchResult.h"

@interface NIKWordSearchResults : NIKSwaggerObject {
@private
    NSNumber* _totalResults; //NSNumber
    NSArray* _searchResults; //WordSearchResult
    }



@property(nonatomic) NSNumber* totalResults;
@property(nonatomic) NSArray* searchResults;
- (id) totalResults: (NSNumber*) totalResults
     searchResults: (NSArray*) searchResults;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

