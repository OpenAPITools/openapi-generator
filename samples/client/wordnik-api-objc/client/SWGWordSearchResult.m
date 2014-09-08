#import "SWGDate.h"
#import "SWGWordSearchResult.h"

@implementation SWGWordSearchResult

-(id)count: (NSNumber*) count
    lexicality: (NSNumber*) lexicality
    word: (NSString*) word
{
  _count = count;
  _lexicality = lexicality;
  _word = word;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _count = dict[@"count"]; 
        _lexicality = dict[@"lexicality"]; 
        _word = dict[@"word"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) dict[@"count"] = _count ;
        if(_lexicality != nil) dict[@"lexicality"] = _lexicality ;
        if(_word != nil) dict[@"word"] = _word ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

