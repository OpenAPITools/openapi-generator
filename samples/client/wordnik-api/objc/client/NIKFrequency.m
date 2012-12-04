#import "NIKDate.h"
#import "NIKFrequency.h"

@implementation NIKFrequency

@synthesize count = _count;
@synthesize year = _year;
- (id) count: (NSNumber*) count
       year: (NSNumber*) year
       {
          _count = count;
          _year = year;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _count = [dict objectForKey:@"count"];
    _year = [dict objectForKey:@"year"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) [dict setObject:_count forKey:@"count"];
    if(_year != nil) [dict setObject:_year forKey:@"year"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

