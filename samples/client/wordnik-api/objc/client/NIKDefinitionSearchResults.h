#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKDefinition.h"

@interface NIKDefinitionSearchResults : NIKSwaggerObject {
@private
    NSArray* _results; //Definition
    NSNumber* _totalResults; //NSNumber
    }



@property(nonatomic) NSArray* results;
@property(nonatomic) NSNumber* totalResults;
- (id) results: (NSArray*) results
     totalResults: (NSNumber*) totalResults;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

