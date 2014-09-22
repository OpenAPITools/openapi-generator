#import "SWGDate.h"
#import "SWGFacetValue.h"

@implementation SWGFacetValue

-(id)count: (NSNumber*) count
    value: (NSString*) value
{
  _count = count;
  _value = value;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _count = dict[@"count"]; 
        _value = dict[@"value"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) dict[@"count"] = _count ;
        if(_value != nil) dict[@"value"] = _value ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

