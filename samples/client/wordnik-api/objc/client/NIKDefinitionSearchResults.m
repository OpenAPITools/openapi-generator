#import "NIKDate.h"
#import "NIKDefinitionSearchResults.h"

@implementation NIKDefinitionSearchResults

@synthesize results = _results;
@synthesize totalResults = _totalResults;
- (id) results: (NSArray*) results
       totalResults: (NSNumber*) totalResults
       {
          _results = results;
          _totalResults = totalResults;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    id results_dict = [dict objectForKey:@"results"];
    if([results_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)results_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)results_dict count]];
            for (NSDictionary* dict in (NSArray*)results_dict) {
                NIKDefinition* d = [[NIKDefinition alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _results = [[NSArray alloc] initWithArray:objs];
        }
    }
    _totalResults = [dict objectForKey:@"totalResults"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_results != nil){
        if([_results isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDefinition * results in (NSArray*)_results) {
                [array addObject:[(NIKSwaggerObject*)results asDictionary]];
            }
            [dict setObject:array forKey:@"results"];
        }
        else if(_results && [_results isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_results toString];
            if(dateString){
                [dict setObject:dateString forKey:@"results"];   
            }
        }
    }
    else {
    if(_results != nil) [dict setObject:[(NIKSwaggerObject*)_results asDictionary]forKey:@"results"];
    }
    if(_totalResults != nil) [dict setObject:_totalResults forKey:@"totalResults"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

