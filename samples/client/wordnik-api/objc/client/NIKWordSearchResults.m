#import "NIKDate.h"
#import "NIKWordSearchResults.h"

@implementation NIKWordSearchResults

@synthesize searchResults = _searchResults;
@synthesize totalResults = _totalResults;
- (id) searchResults: (NSArray*) searchResults
       totalResults: (NSNumber*) totalResults
       {
          _searchResults = searchResults;
          _totalResults = totalResults;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    id searchResults_dict = [dict objectForKey:@"searchResults"];
    if([searchResults_dict isKindOfClass:[NSArray class]]) {
        if([(NSArray*)searchResults_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)searchResults_dict count]];
            for (NSDictionary* dict in (NSArray*)searchResults_dict) {
                NIKWordSearchResult* d = [[NIKWordSearchResult alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _searchResults = [[NSArray alloc] initWithArray:objs];
        }
    }
    _totalResults = [dict objectForKey:@"totalResults"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_searchResults != nil){
        if([_searchResults isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKWordSearchResult * searchResults in (NSArray*)_searchResults) {
                [array addObject:[(NIKSwaggerObject*)searchResults asDictionary]];
            }
            [dict setObject:array forKey:@"searchResults"];
        }
        else if(_searchResults && [_searchResults isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_searchResults toString];
            if(dateString){
                [dict setObject:dateString forKey:@"searchResults"];   
            }
        }
    }
    else {
    if(_searchResults != nil) [dict setObject:[(NIKSwaggerObject*)_searchResults asDictionary]forKey:@"searchResults"];
    }
    if(_totalResults != nil) [dict setObject:_totalResults forKey:@"totalResults"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

