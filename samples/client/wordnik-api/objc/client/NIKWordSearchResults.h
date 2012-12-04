#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKWordSearchResult.h"

@interface NIKWordSearchResults : NIKSwaggerObject {
@private
    NSArray* _searchResults; //WordSearchResult
    NSNumber* _totalResults; //NSNumber
    }



@property(nonatomic) NSArray* searchResults;
@property(nonatomic) NSNumber* totalResults;
- (id) searchResults: (NSArray*) searchResults
     totalResults: (NSNumber*) totalResults;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

