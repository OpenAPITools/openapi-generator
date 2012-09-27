#import "NIKDate.h"
#import "NIKFacetValue.h"

@implementation NIKFacetValue

@synthesize count = _count;
@synthesize value = _value;
- (id) count: (NSNumber*) count
       value: (NSString*) value
       {
          _count = count;
          _value = value;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    _count = [dict objectForKey:@"count"];
    _value = [dict objectForKey:@"value"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_count != nil) [dict setObject:_count forKey:@"count"];
    if(_value != nil) [dict setObject:_value forKey:@"value"];
    NSDictionary* output = [dict copy];
    return output;
}

@end

