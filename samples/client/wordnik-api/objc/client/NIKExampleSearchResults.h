#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKFacet.h"
#import "NIKExample.h"

@interface NIKExampleSearchResults : NIKSwaggerObject {
@private
    NSArray* _facets; //Facet
    NSArray* _examples; //Example
    }



@property(nonatomic) NSArray* facets;
@property(nonatomic) NSArray* examples;
- (id) facets: (NSArray*) facets
     examples: (NSArray*) examples;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

