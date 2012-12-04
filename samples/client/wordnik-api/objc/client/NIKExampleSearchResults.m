#import "NIKDate.h"
#import "NIKExampleSearchResults.h"

@implementation NIKExampleSearchResults

@synthesize facets = _facets;
@synthesize examples = _examples;
- (id) facets: (NSArray*) facets
       examples: (NSArray*) examples
       {
          _facets = facets;
          _examples = examples;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    id facets_dict = [dict objectForKey:@"facets"];
    if([facets_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)facets_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)facets_dict count]];
            for (NSDictionary* dict in (NSArray*)facets_dict) {
                NIKFacet* d = [[NIKFacet alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _facets = [[NSArray alloc] initWithArray:objs];
        }
    }
    id examples_dict = [dict objectForKey:@"examples"];
    if([examples_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)examples_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)examples_dict count]];
            for (NSDictionary* dict in (NSArray*)examples_dict) {
                NIKExample* d = [[NIKExample alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _examples = [[NSArray alloc] initWithArray:objs];
        }
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_facets != nil){
        if([_facets isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKFacet * facets in (NSArray*)_facets) {
                [array addObject:[(NIKSwaggerObject*)facets asDictionary]];
            }
            [dict setObject:array forKey:@"facets"];
        }
        else if(_facets && [_facets isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_facets toString];
            if(dateString){
                [dict setObject:dateString forKey:@"facets"];   
            }
        }
    }
    else {
    if(_facets != nil) [dict setObject:[(NIKSwaggerObject*)_facets asDictionary]forKey:@"facets"];
    }
    if(_examples != nil){
        if([_examples isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKExample * examples in (NSArray*)_examples) {
                [array addObject:[(NIKSwaggerObject*)examples asDictionary]];
            }
            [dict setObject:array forKey:@"examples"];
        }
        else if(_examples && [_examples isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_examples toString];
            if(dateString){
                [dict setObject:dateString forKey:@"examples"];   
            }
        }
    }
    else {
    if(_examples != nil) [dict setObject:[(NIKSwaggerObject*)_examples asDictionary]forKey:@"examples"];
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

