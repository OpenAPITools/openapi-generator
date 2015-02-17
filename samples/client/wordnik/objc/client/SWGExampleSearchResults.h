#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGExample.h"
#import "SWGFacet.h"


@interface SWGExampleSearchResults : SWGObject

@property(nonatomic) NSArray* facets;  
@property(nonatomic) NSArray* examples;  
- (id) facets: (NSArray*) facets     
    examples: (NSArray*) examples;
    

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end
