#import <Foundation/Foundation.h>
#import "SWGObject.h"
#import "SWGFacetValue.h"


@interface SWGFacet : SWGObject

@property(nonatomic) NSArray* facetValues;  

@property(nonatomic) NSString* name;  

- (id) facetValues: (NSArray*) facetValues
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

