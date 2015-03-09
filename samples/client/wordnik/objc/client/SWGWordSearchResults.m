#import "SWGDate.h"
#import "SWGWordSearchResults.h"

@implementation SWGWordSearchResults

-(id)searchResults: (NSArray*) searchResults
    totalResults: (NSNumber*) totalResults
    
{
    _searchResults = searchResults;
    _totalResults = totalResults;
    

    return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        
        
        id searchResults_dict = dict[@"searchResults"];
        
        if([searchResults_dict isKindOfClass:[NSArray class]]) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)searchResults_dict count]];
            if([(NSArray*)searchResults_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)searchResults_dict) {
                    SWGWordSearchResult* d = [[SWGWordSearchResult alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                _searchResults = [[NSArray alloc] initWithArray:objs];
            }
            else
                _searchResults = [[NSArray alloc] init];
        }
        else {
            _searchResults = [[NSArray alloc] init];
        }
        
        
        _totalResults = dict[@"totalResults"];
        
        
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    
    
    if(_searchResults != nil){
        if([_searchResults isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( SWGWordSearchResult *searchResults in (NSArray*)_searchResults) {
                [array addObject:[(SWGObject*)searchResults asDictionary]];
            }
            dict[@"searchResults"] = array;
        }
        else if(_searchResults && [_searchResults isKindOfClass:[SWGDate class]]) {
            NSString * dateString = [(SWGDate*)_searchResults toString];
            if(dateString){
                dict[@"searchResults"] = dateString;
            }
        }
        else {
        
            if(_searchResults != nil) dict[@"searchResults"] = [(SWGObject*)_searchResults asDictionary];
        
        }
    }
    
    
    
            if(_totalResults != nil) dict[@"totalResults"] = _totalResults ;
        
    

    NSDictionary* output = [dict copy];
    return output;
}

@end
