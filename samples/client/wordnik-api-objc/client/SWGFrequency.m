#import "SWGDate.h"
#import "SWGFrequency.h"

@implementation SWGFrequency

-(id)count: (NSNumber*) count
    year: (NSNumber*) year
{
  _count = count;
  _year = year;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _count = dict[@"count"]; 
        _year = dict[@"year"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) dict[@"count"] = _count ;
        if(_year != nil) dict[@"year"] = _year ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

