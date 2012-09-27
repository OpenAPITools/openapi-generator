#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKFacetValue.h"

@interface NIKFacet : NIKSwaggerObject {
@private
    NSArray* _facetValues; //FacetValue
    NSString* _name; //NSString
    }



@property(nonatomic) NSArray* facetValues;
@property(nonatomic) NSString* name;
- (id) facetValues: (NSArray*) facetValues
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

