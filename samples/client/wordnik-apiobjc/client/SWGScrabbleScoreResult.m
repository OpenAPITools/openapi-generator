#import "SWGDate.h"
#import "SWGScrabbleScoreResult.h"

@implementation SWGScrabbleScoreResult

-(id)value: (NSNumber*) value
{
  _value = value;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _value = dict[@"value"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_value != nil) dict[@"value"] = _value ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

