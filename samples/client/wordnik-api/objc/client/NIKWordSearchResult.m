#import "NIKDate.h"
#import "NIKWordSearchResult.h"

@implementation NIKWordSearchResult

@synthesize count = _count;
@synthesize lexicality = _lexicality;
@synthesize word = _word;
- (id) count: (NSNumber*) count
       lexicality: (NSNumber*) lexicality
       word: (NSString*) word
       {
          _count = count;
          _lexicality = lexicality;
          _word = word;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _count = [dict objectForKey:@"count"];
    _lexicality = [dict objectForKey:@"lexicality"];
    _word = [dict objectForKey:@"word"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) [dict setObject:_count forKey:@"count"];
    if(_lexicality != nil) [dict setObject:_lexicality forKey:@"lexicality"];
    if(_word != nil) [dict setObject:_word forKey:@"word"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

