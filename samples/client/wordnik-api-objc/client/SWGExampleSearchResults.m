#import "SWGDate.h"
#import "SWGExampleSearchResults.h"

@implementation SWGExampleSearchResults

-(id)facets: (NSArray*) facets
    examples: (NSArray*) examples
{
  _facets = facets;
  _examples = examples;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        id facets_dict = dict[@"facets"];
        if([facets_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)facets_dict count]];

            if([(NSArray*)facets_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)facets_dict) {
                    SWGFacet* d = [[SWGFacet alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _facets = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _facets = [[NSArray alloc] init];
            }
        }
        else {
            _facets = [[NSArray alloc] init];
        }
        id examples_dict = dict[@"examples"];
        if([examples_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)examples_dict count]];

            if([(NSArray*)examples_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)examples_dict) {
                    SWGExample* d = [[SWGExample alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _examples = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _examples = [[NSArray alloc] init];
            }
        }
        else {
            _examples = [[NSArray alloc] init];
        }
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_facets != nil){
        if([_facets isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGFacet *facets in (NSArray*)_facets) {
                [array addObject:[(SWGObject*)facets asDictionary]];
            }
            dict[@"facets"] = array;
        }
        else if(_facets && [_facets isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_facets toString];
            if(dateString){
                dict[@"facets"] = dateString;
            }
        }
        else {
        if(_facets != nil) dict[@"facets"] = [(SWGObject*)_facets asDictionary];
        }
    }
    if(_examples != nil){
        if([_examples isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGExample *examples in (NSArray*)_examples) {
                [array addObject:[(SWGObject*)examples asDictionary]];
            }
            dict[@"examples"] = array;
        }
        else if(_examples && [_examples isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_examples toString];
            if(dateString){
                dict[@"examples"] = dateString;
            }
        }
        else {
        if(_examples != nil) dict[@"examples"] = [(SWGObject*)_examples asDictionary];
        }
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

