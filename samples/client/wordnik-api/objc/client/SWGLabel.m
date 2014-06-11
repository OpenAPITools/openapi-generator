#import "SWGDate.h"
#import "SWGLabel.h"

@implementation SWGLabel

-(id)text: (NSString*) text
    type: (NSString*) type
{
  _text = text;
  _type = type;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _text = dict[@"text"]; 
        _type = dict[@"type"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_text != nil) dict[@"text"] = _text ;
        if(_type != nil) dict[@"type"] = _type ;
        NSDictionary* output = [dict copy];
    return output;
}

@end

