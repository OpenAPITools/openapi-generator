#import "NIKDate.h"
#import "NIKFacet.h"

@implementation NIKFacet

@synthesize facetValues = _facetValues;
@synthesize name = _name;
- (id) facetValues: (NSArray*) facetValues
       name: (NSString*) name
       {
          _facetValues = facetValues;
          _name = name;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    id facetValues_dict = [dict objectForKey:@"facetValues"];
    if([facetValues_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)facetValues_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)facetValues_dict count]];
            for (NSDictionary* dict in (NSArray*)facetValues_dict) {
                NIKFacetValue* d = [[NIKFacetValue alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _facetValues = [[NSArray alloc] initWithArray:objs];
        }
    }
    _name = [dict objectForKey:@"name"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_facetValues != nil){
        if([_facetValues isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKFacetValue * facetValues in (NSArray*)_facetValues) {
                [array addObject:[(NIKSwaggerObject*)facetValues asDictionary]];
            }
            [dict setObject:array forKey:@"facetValues"];
        }
        else if(_facetValues && [_facetValues isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_facetValues toString];
            if(dateString){
                [dict setObject:dateString forKey:@"facetValues"];   
            }
        }
    }
    else {
    if(_facetValues != nil) [dict setObject:[(NIKSwaggerObject*)_facetValues asDictionary]forKey:@"facetValues"];
    }
    if(_name != nil) [dict setObject:_name forKey:@"name"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

