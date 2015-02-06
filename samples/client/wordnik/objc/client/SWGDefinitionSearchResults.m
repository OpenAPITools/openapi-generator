#import "SWGDate.h"
#import "SWGDefinitionSearchResults.h"

@implementation SWGDefinitionSearchResults

-(id)results: (NSArray*) results
    totalResults: (NSNumber*) totalResults
    
{
    _results = results;
    _totalResults = totalResults;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        
        
        id results_dict = dict[@"results"];
        
        if([results_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)results_dict count]];
            if([(NSArray*)results_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)results_dict) {
                    SWGDefinition* d = [[SWGDefinition alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _results = [[NSArray alloc] initWithArray:objs];
            }
            else
                _results = [[NSArray alloc] init];
        }
        else {
            _results = [[NSArray alloc] init];
        }
        
        
        _totalResults = dict[@"totalResults"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
    if(_results != nil){
        if([_results isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGDefinition *results in (NSArray*)_results) {
                [array addObject:[(SWGObject*)results asDictionary]];
            }
            dict[@"results"] = array;
        }
        else if(_results && [_results isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_results toString];
            if(dateString){
                dict[@"results"] = dateString;
            }
        }
        else {
        
            if(_results != nil) dict[@"results"] = [(SWGObject*)_results asDictionary];
        
        }
    }
    
    
    
            if(_totalResults != nil) dict[@"totalResults"] = _totalResults ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
