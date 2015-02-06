#import "SWGDate.h"
#import "SWGFacet.h"

@implementation SWGFacet

-(id)facetValues: (NSArray*) facetValues
    name: (NSString*) name
    
{
    _facetValues = facetValues;
    _name = name;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        
        
        id facetValues_dict = dict[@"facetValues"];
        
        if([facetValues_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)facetValues_dict count]];
            if([(NSArray*)facetValues_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)facetValues_dict) {
                    SWGFacetValue* d = [[SWGFacetValue alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _facetValues = [[NSArray alloc] initWithArray:objs];
            }
            else
                _facetValues = [[NSArray alloc] init];
        }
        else {
            _facetValues = [[NSArray alloc] init];
        }
        
        
        _name = dict[@"name"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
    if(_facetValues != nil){
        if([_facetValues isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGFacetValue *facetValues in (NSArray*)_facetValues) {
                [array addObject:[(SWGObject*)facetValues asDictionary]];
            }
            dict[@"facetValues"] = array;
        }
        else if(_facetValues && [_facetValues isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_facetValues toString];
            if(dateString){
                dict[@"facetValues"] = dateString;
            }
        }
        else {
        
            if(_facetValues != nil) dict[@"facetValues"] = [(SWGObject*)_facetValues asDictionary];
        
        }
    }
    
    
    
            if(_name != nil) dict[@"name"] = _name ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
